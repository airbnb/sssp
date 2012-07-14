{-# LANGUAGE OverloadedStrings
           , GADTs
           , StandaloneDeriving
  #-}
module AWS.S3P where

import           Control.Applicative
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Word
import           System.IO

import qualified Aws as Aws
import qualified Aws.S3 as Aws
import           Data.Attempt
import qualified Network.HTTP.Conduit as Conduit


parse' :: Char -> [Text] -> ParsedRequest
parse' meta = foldr (Singular [], [])
 where
  f s (url
  wildcard text = List.elem text strings 
   where
    strings = Text.cons meta . fst <$> wildcards


data Ordering    = ASCII | SemVer
data Wildcard    = Hi Ordering | Lo Ordering
data SetWildcard = Include Word Wildcard | Exclude Word Wildcard


-- | URLs are either singular or plural in character. Those ending in @/@ or 
--   containing set wildcards are plural; other URLs are singular. A singular
--   URL is translated to a redirect while a plural URL is returned as a
--   newline-separated list of URLs.
data URL t where
  Singular :: [Either Text Wildcard] -> URL Redirect
  Plural :: [Either Text (Either Wildcard SetWildcard)] -> URL [Text]
deriving instance Show (URL t)

-- | A datatype that represents the result of request parsing.
data ParsedRequest = forall t. ParsedRequest (URL t) 
deriving instance Show ParsedRequest

wildcards = [("hi"       , Hi ASCII)
            ,("lo"       , Lo ASCII)
            ,("hi.ascii" , Hi ASCII)
            ,("lo.ascii" , Lo ASCII)
            ,("hi.semver", Hi SemVer)
            ,("lo.semver", Lo SemVer)]

resolve :: Ctx -> [Component] -> IO [[Text]]
resolve Ctx{...} = resolve' []
 where
  resolve' acc [   ] = [reverse acc]
  resolve' acc (h:t) = case h of
    Plain text -> resolve' (text:acc) t
    Meta ... -> do
      let prefix = (Text.intercalate "/" . reverse) ("/":acc)
          gb = Aws.GetBucket { Aws.gbBucket = bucket
                             , Aws.gbPrefix = Just prefix
                             , Aws.gbDelimiter = Just "/" }
      Aws.Response meta attempt <- Aws.aws aws s3 manager gb 
      case attempt of
        -- Should return an error term here.
        Failure e -> err "Request failed." >> return []
        Success gbr -> do
          let names   = [...]
              newAccs = expand sw names
          List.concat <$> mapM (resolve' _ t) newAccs

data Ctx = Ctx { bucket :: Aws.Bucket
               , aws :: Aws.Configuration
               , s3 :: Aws.S3Configuration
               , manager :: Conduit.Manager }

expand :: (Ord o) => [o] -> SetWildcard -> [o]
expand sw items = case specifier of
  Complement sw' -> complementList (expand sw' set) 
  Counted Lo n   -> List.take n sorted
  Counted Hi n   -> (List.reverse . List.take n . List.reverse) sorted
 where
  set = Set.fromAscList sorted
  sorted = List.sort items
  complementList = Set.toAscList . Set.difference set . Set.fromAscList

err = hPutStrLn stderr

