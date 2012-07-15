{-# LANGUAGE OverloadedStrings
           , GADTs
           , StandaloneDeriving
  #-}
module AWS.S3P where

import           Control.Applicative
import           Data.Either
import qualified Data.List as List
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Word
import           System.IO

import qualified Aws as Aws
import qualified Aws.S3 as Aws
import           Data.Attempt
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Conduit as Conduit


data Order       = ASCII | SemVer deriving (Eq, Ord, Show)
data Wildcard    = Hi Order | Lo Order deriving (Eq, Ord, Show)
data SetWildcard = Include Word Wildcard | Exclude Word Wildcard
 deriving (Eq, Ord, Show)

-- | URLs are either singular or plural in character. Those ending in @/@ or
--   containing set wildcards are plural; other URLs are singular. A singular
--   URL is translated to a redirect while a plural URL is returned as a
--   newline-separated list of URLs.
data URL t where
  Singular :: [Either Text Wildcard] -> URL Redirect
  Plural :: [Either (Either Text Wildcard) SetWildcard] -> URL [Text]
deriving instance Eq (URL t)
deriving instance Ord (URL t)
deriving instance Show (URL t)

newtype Redirect = Redirect Text deriving (Eq, Ord, Show)


-- | A datatype that represents the result of request parsing.
data ParsedRequest = forall t. ParsedRequest
  Char -- ^ Meta character chosen for this parse.
  (URL t) -- ^ Resultant URL, singular or plural.
deriving instance Show ParsedRequest

url :: Char -> [Text] -> ParsedRequest
url meta texts | singular  = ParsedRequest meta (Singular (lefts components))
               | otherwise = ParsedRequest meta (Plural components)
 where
  (empty, full) = List.break (/= "") . List.reverse $ texts
  components = parse <$> List.reverse full
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
wildcards = [("hi.semver", Hi SemVer)
            ,("lo.semver", Lo SemVer)
            ,("hi.ascii" , Hi ASCII)
            ,("lo.ascii" , Lo ASCII)
            ,("hi"       , Hi ASCII)
            ,("lo"       , Lo ASCII)]
-- The order of these matters when they are translated to alternative
-- Attoparsec parsers, which is unfortunate and seemingly contrary to the
-- documentation. In lieu of left-factoring, we put the prefixes last.


-- resolve :: Ctx -> [Component] -> IO [[Text]]
-- resolve Ctx{...} = resolve' []
--  where
--   resolve' acc [   ] = [reverse acc]
--   resolve' acc (h:t) = case h of
--     Plain text -> resolve' (text:acc) t
--     Meta ... -> do
--       let prefix = (Text.intercalate "/" . reverse) ("/":acc)
--           gb = Aws.GetBucket { Aws.gbBucket = bucket
--                              , Aws.gbPrefix = Just prefix
--                              , Aws.gbDelimiter = Just "/" }
--       Aws.Response meta attempt <- Aws.aws aws s3 manager gb
--       case attempt of
--         -- Should return an error term here.
--         Failure e -> err "Request failed." >> return []
--         Success gbr -> do
--           let names   = [...]
--               newAccs = expand sw names
--           List.concat <$> mapM (resolve' _ t) newAccs

-- data Ctx = Ctx { bucket :: Aws.Bucket
--                , aws :: Aws.Configuration
--                , s3 :: Aws.S3Configuration
--                , manager :: Conduit.Manager }

-- expand :: (Ord o) => [o] -> SetWildcard -> [o]
-- expand sw items = case specifier of
--   Complement sw' -> complementList (expand sw' set)
--   Counted Lo n   -> List.take n sorted
--   Counted Hi n   -> (List.reverse . List.take n . List.reverse) sorted
--  where
--   set = Set.fromAscList sorted
--   sorted = List.sort items
--   complementList = Set.toAscList . Set.difference set . Set.fromAscList

err = hPutStrLn stderr

