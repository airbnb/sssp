{-# LANGUAGE OverloadedStrings
           , GADTs
           , RecordWildCards
           , StandaloneDeriving
           , ExistentialQuantification
  #-}
module Aws.S3P where

import           Control.Applicative
import           Data.Char
import           Data.Either
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.List as List
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as Set
import           Data.Word
import           System.IO

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Network.Wai as WWW
import qualified Network.Wai.Handler.Warp as WWW
import qualified Network.HTTP.Types as HTTP

import qualified Aws as Aws
import qualified Aws.S3 as Aws
import           Data.Attempt
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Network.HTTP.Conduit as Conduit


-- s3p :: Ctx -> WWW.Application
-- s3p ctx req@WWW.Request{..} = ...

-- | Resources are either singular or plural in character. URLs ending ending
--   in @/@ or containing set wildcards specify plural resources; all other
--   URLs indicate singular resources. A singular resource results in a
--   redirect while a plural resource results in a newline-separated list of
--   URLs (themselves singular in character).
data Resource = Singular [Either Text Wildcard]
              | Plural [Either (Either Text Wildcard) SetWildcard]
 deriving (Eq, Ord, Show)

data Ctx = Ctx { bucket :: Aws.Bucket
               , aws :: Aws.Configuration
               , s3 :: Aws.S3Configuration
               , manager :: Conduit.Manager }
instance Show Ctx where
  show Ctx{..} = mconcat [ "Ctx { bucket=", show bucket
                         , ", aws=..., s3=", show s3, " }" ]

newtype Redirect = Redirect Text deriving (Eq, Ord, Show)

data Order       = ASCII | SemVer deriving (Eq, Ord, Show)
data Wildcard    = Hi Order | Lo Order deriving (Eq, Ord, Show)
data SetWildcard = Include Word Wildcard | Exclude Word Wildcard
 deriving (Eq, Ord, Show)

-- | Interpret a request URL as a resource, expanding wildcards as needed. By
--   default, wildcards are expanded with @\@@ as the meta-character (@\@hi@,
--   @\@lo.semver5@) but the meta-character can be changed with a query
--   parameter so we pass the whole request here.
--
--   The meta-character is in leading position in wildcard path components and
--   escapes itself in leading position, in a simple way: leading runs are
--   shortened by one character. Some examples of path components and their
--   interpretation are helpful:
-- @
--    hi      -> The string "hi".
--    @hi     -> The hi.ascii wildcard.
--    @@hi    -> The string "@hi".
--    @@@hi   -> The string "@@hi".
--    ...and so on...
-- @
--   Sending @meta=_@ as a query parameter changes the meta-character to an
--   underscore. The meta-character may be any single character; empty or
--   overlong @meta@ parameters are ignored.
resource :: WWW.Request -> Resource
resource WWW.Request{..} = url metaChar pathInfo
 where
  metaParams = [ b | Just (b, _) <- culled ] :: [Char]
   where culled = [ Bytes.uncons v | (k, Just v) <- queryString, k == "meta" ]
  metaChar = List.head (metaParams ++ ['@'])

url :: Char -> [Text] -> Resource
url meta texts | singular    = Singular (lefts components)
               | otherwise   = Plural components
 where
  (empty, full) = List.break (/= "") . List.reverse $ texts
  empty' = if empty /= [] then [""] else []
  components = parse <$> List.reverse (empty' ++ full)
  singular = empty == [] && rights components == []
  -- Parser is total but just to be on the safe side...
  parse t = either (const . Left . Left $ t) id
                   (Atto.parseOnly (component meta) t)

-- | Parse a single path component.
component :: Char -> Parser (Either (Either Text Wildcard) SetWildcard)
component meta = eitherRotate <$> Atto.eitherP
  (Atto.eitherP (setWildcard meta) (wildcard meta) <* Atto.endOfInput)
  (plain meta)
 where
  eitherRotate :: Either (Either SetWildcard Wildcard) Text
               -> Either (Either Text Wildcard) SetWildcard
  eitherRotate (Left (Right wc)) = Left (Right wc)
  eitherRotate (Left (Left set)) = Right set
  eitherRotate (Right text)      = Left (Left text)

-- | Parse a plain string, shrinking leading runs of the metacharacter by one.
plain :: Char -> Parser Text
plain c = mappend <$> (Text.drop 1 <$> Atto.takeWhile (== c)) <*> Atto.takeText

-- | Match a simple, singular wildcard.
wildcard :: Char -> Parser Wildcard
wildcard meta = Atto.char meta *> Atto.choice matchers
 where
  matcher (t, w) = Atto.string t *> pure w
  matchers       = matcher <$> wildcards

-- | Match a wildcard set, ending with a count (if it is inclusive) or an
--   optional count and a final tilde (if it is exclusive).
setWildcard :: Char -> Parser SetWildcard
setWildcard meta = wildcard meta <**> (exclude <|> include)
 where
  include = Include <$> Atto.decimal
  exclude = Exclude <$> Atto.option 1 Atto.decimal <* Atto.char '~'

-- | Wildcards and their textual representations.
wildcards :: [(Text, Wildcard)]
wildcards = [("hi.semver", Hi SemVer) ,("lo.semver", Lo SemVer)
            ,( "hi.ascii", Hi ASCII)  ,( "lo.ascii", Lo ASCII)
            ,(       "hi", Hi ASCII)  ,(       "lo", Lo ASCII)]
-- The order of these matters when they are translated to alternative
-- Attoparsec parsers, which is unfortunate and seemingly contrary to the
-- documentation. In lieu of left-factoring, we put the longer prefixes last.


resolve :: Ctx -> Resource -> IO (Attempt [Text])
resolve Ctx{..} res = case res of
  Singular components -> resolve' "" (pluralize <$> components)
  Plural components   -> resolve' "" (simplify <$> components)
 where
  pluralize :: Either Text Wildcard -> Either Text SetWildcard
  pluralize = either Left (Right . Include 1)
  simplify :: Either (Either Text Wildcard) SetWildcard
           -> Either Text SetWildcard
  simplify = either pluralize Right
  resolve' :: Text -> [Either Text SetWildcard] -> IO (Attempt [Text])
  resolve' prefix [   ] = return (Success [prefix])
  resolve' prefix (h:t) = case h of
    Left ""   -> (fst <$>) <$> listing Ctx{..} prefix
    Left text -> resolve' (prefix -/- text) t
    Right set -> do
      attempt <- listing Ctx{..} prefix
      case names <$> attempt of
        Success texts -> (List.concat <$>) . sequence <$> mapM recurse texts
        Failure e -> return (Failure e)
     where
      names (objects, prefixes) = expand set $ case t of [ ] -> objects
                                                         _:_ -> prefixes
      recurse text = resolve' text t


listing :: Ctx -> Text -> IO (Attempt ([Text],[Text]))
listing Ctx{..} prefix = do
  -- For now, we don't do any results paging, limiting ourselves to the
  -- first thousand results, as per the Amazon maximums.
  Aws.Response _meta attempt <- Aws.aws aws s3 manager gb
  return $ do
    Aws.GetBucketResponse{..} <- attempt
    Success (Aws.objectKey <$> gbrContents, gbrCommonPrefixes)
 where
  gb = Aws.GetBucket { Aws.gbBucket = bucket
                     , Aws.gbPrefix = Just (prefix -/- "")
                     , Aws.gbDelimiter = Just "/"
                     , Aws.gbMaxKeys = Nothing
                     , Aws.gbMarker = Nothing }

expand :: SetWildcard -> [Text] -> [Text]
expand set texts = if complement then complemented matching else matching
 where
  matching = (selected . ordered) texts
  uniq = Set.fromList texts
  complemented = ordered . Set.toList . Set.difference uniq . Set.fromList
  (count, wc, complement) = case set of
    Include count wc -> (fromIntegral count, wc, False)
    Exclude count wc -> (fromIntegral count, wc, True)
  (ordered, selected) = case wc of
    Hi o -> (order o, List.reverse . List.take count . List.reverse)
    Lo o -> (order o, List.take count)

a -/- b | a == ""                 = mappend a b
        | "/" `Text.isSuffixOf` a = mappend a b
        | otherwise               = mconcat [a, "/", b]

order :: Order -> [Text] -> [Text]
order ASCII  = List.sort
order SemVer = List.sortBy (comparing textSemVer)

textSemVer :: Text -> [Integer]
textSemVer = (fst <$>) . rights . (Text.decimal <$>) . digitalPieces
 where
  digitalPieces = List.filter (/= "") . Text.split (not . isDigit)

err = hPutStrLn stderr

