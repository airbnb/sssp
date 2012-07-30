{-# LANGUAGE OverloadedStrings
           , ParallelListComp
           , RecordWildCards
           , StandaloneDeriving
           , PatternGuards
  #-}
module Aws.SSSP where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Char
import           Data.Either
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as Set
import           Data.Word

import qualified Aws as Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as Aws
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import           Data.Attempt
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Conduit as Conduit
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Network.Wai as WWW
import qualified Network.HTTP.Conduit as Conduit
import qualified Network.HTTP.Types as HTTP


wai :: Ctx -> WWW.Application
wai ctx@Ctx{..} req@WWW.Request{..} = do
  resolved <- liftIO $ task ctx requestMethod (resource req)
  maybe (return badTask) id $ do
    task <- resolved
    Just $ case task of
      Redirect t -> do
        n <- case [ toSignTime v | (k, Just v) <- queryString, k == "t" ] of
               [              ] -> return 10
               Just seconds : _ -> return seconds
               Nothing      : _ -> return 2 -- TODO: Bad param message.
        sigInfo <- liftIO $ sigData (fromIntegral n)
        let q = Aws.getObject bucket t Aws.s3ErrorResponseConsumer
            s = Aws.queryToUri (Aws.signQuery q s3 sigInfo)
            b = Blaze.fromByteString (s `Bytes.snoc` '\n')
            m = "max-age=" ++ show (n - 1)
            headers = [("Cache-Control", Bytes.pack m)
                      ,("Content-Type", "text/plain")
                      ,("Location", s)]
        return $ WWW.ResponseBuilder status307 headers b
      Listing ts -> do
        return $ WWW.ResponseBuilder
          HTTP.status200 [("Content-Type", "text/plain")]
                         (mconcat (plusNL . s3Encode <$> ts))
      Remove ts  -> do
        let deletes = [ Aws.DeleteObject t bucket | t <- ts ]
        responses <- mapM (Aws.aws aws s3 manager) deletes
        let attempts = [ attempt | Aws.Response _meta attempt <- responses ]
            d = [ mappend `uncurry` case a of
                    Success _ -> ("deleted: ", t)
                    Failure _ -> ("failed:  ", t) | a <- attempts | t <- ts ]
            status | all isSuccess attempts = HTTP.status200
                   | otherwise              = HTTP.status500
        return $ WWW.ResponseBuilder
          status [("Content-Type", "text/plain")]
                 (Blaze.fromText . Text.unlines $ d)
      Write t    -> do
        let len = join $ listToMaybe
                  [ fst <$> (listToMaybe . reads . Bytes.unpack) v
                  | (k, v) <- requestHeaders, k == "Content-Length" ]
        maybe (return noLength) id $ do
          n <- len
          let po = Aws.putObject bucket t (blazeBody n)
          Just $ do
            Aws.Response _meta attempt <- Aws.aws aws s3 manager po
            let status | isSuccess attempt = HTTP.status200
                       | otherwise         = HTTP.status500
            return $ WWW.ResponseBuilder
              status [("Content-Type", "text/plain")] mempty
 where
  status307 = HTTP.Status 307 "Temporary Redirect"
  sigData n = Aws.signatureData (Aws.ExpiresIn n) (Aws.credentials aws)
  badTask = WWW.ResponseBuilder
    HTTP.status400 [("Content-Type", "text/plain")]
                   (Blaze.fromByteString "Malformed task.\n")
  noLength = WWW.ResponseBuilder
    HTTP.status400 [("Content-Type", "text/plain")]
                   (Blaze.fromByteString "No Content-Length header.\n")
  blazeBody len = Conduit.RequestBodySource len
                . Conduit.mapOutput (Blaze.fromByteString) $ requestBody
  toSignTime :: ByteString -> Maybe Integer
  toSignTime  = (validate =<<) . (fst <$>) . listToMaybe . reads . Bytes.unpack
   where
    validate i   = guard (notTooMany i) >> Just i
    notTooMany i = i >= 2 && i <= 10000000 -- 4 months

task :: Ctx -> HTTP.Method -> Resource -> IO (Maybe Task)
task ctx m r
  | "GET"    <- m, Singular _ <- r = (Redirect <$>) . (listToMaybe =<<)
                                  <$> resolved
  | "GET"    <- m, Plural   _ <- r = (Listing <$>) <$> resolved
  | "DELETE" <- m                  = (Remove <$>) <$> resolved
  | "PUT"    <- m, Singular _ <- r = (Write <$>) . (listToMaybe =<<)
                                  <$> resolved
  | otherwise                      = return Nothing
 where
  resolved = fromAttempt <$> resolve ctx r

data Task = Redirect Text
          | Listing [Text]
          | Remove [Text]
          | Write Text -- (Conduit.RequestBody IO)
 deriving (Eq, Ord, Show)

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
--    @hi     -> The hi.semver wildcard.
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
url _    []                  = Plural []
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
wildcards = [( "hi.ascii", Hi ASCII)  ,( "lo.ascii", Lo ASCII)
            ,("hi.semver", Hi SemVer) ,("lo.semver", Lo SemVer)
            ,(       "hi", Hi SemVer) ,(       "lo", Lo SemVer)]
-- The order of these matters when they are translated to alternative
-- Attoparsec parsers, which is unfortunate and seemingly contrary to the
-- documentation. In lieu of left-factoring, we put the longer prefixes last.


-- | Translate a resource in to a listing of objects. While intermediate S3
--   prefixes (directories) are traversed, the final match is always on keys
--   for objects.
resolve :: Ctx -> Resource -> IO (Attempt [Text])
resolve Ctx{..} res = case res of
  Plural [ ]          -> (fst <$>) <$> listing Ctx{..} ""
  Plural components   -> resolve' "" (simplify <$> components)
  Singular components -> resolve' "" (pluralize <$> components)
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
      recurse text = resolve' text t
      names (objects, prefixes) = expand set $ case t of [ ] -> objects
                                                         _:_ -> prefixes

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

-- | Split a URL into components, placing the balance of slashes in a slash
--   run to the left of the last slash. This is what all the Amazon APIs --
--   including the HTTP interface -- seem to expect, based on experiment.
--   This function exists so that we can split a URL retrieved from S3, by way
--   of list bucket, for example, into pieces for later escaping.
s3Pieces :: Text -> [Text]
s3Pieces text = reverse . uncurry (:) $ List.foldl' f (leading', []) rest'
 where
  (leading, rest) = List.span (=="") (Text.split (=='/') text)
  leading'' = Text.pack [ '/' | _ <- leading ]
  (leading', rest') | h:t <- rest = (mappend leading'' h, t)
                    | otherwise   = (leading'', [])
  f (piece, pieces) "" = (piece `Text.snoc` '/', pieces)
  f (piece, pieces) s  = (s, piece:pieces)

-- | Encode an S3 path to a URL, splitting on slashes but preserving slash
--   runs as appropriate.
s3Encode :: Text -> Blaze.Builder
s3Encode  = HTTP.encodePathSegmentsRelative . s3Pieces

plusNL :: Blaze.Builder -> Blaze.Builder
plusNL  = (`mappend` Blaze.fromChar '\n')

order :: Order -> [Text] -> [Text]
order ASCII  = List.sort
order SemVer = List.sortBy (comparing textSemVer)

textSemVer :: Text -> [Integer]
textSemVer = (fst <$>) . rights . (Text.decimal <$>) . digitalPieces
 where
  digitalPieces = List.filter (/= "") . Text.split (not . isDigit)

