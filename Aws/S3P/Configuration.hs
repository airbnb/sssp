{-# LANGUAGE OverloadedStrings
           , TupleSections #-}
-- | Utilities for determining the server configuration from environment
--   variables and file input.
module Aws.S3P.Configuration where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           System.Environment
import           System.IO
import           System.IO.Error

import qualified Aws as Aws
import qualified Aws.S3 as Aws
import           Data.Attoparsec.Char8


-- | Interpret a region name, like @us-west-1@, in accord with the Amazon's
--   documentation for endpoint lcoations.
--   <http://docs.amazonwebservices.com/general/latest/gr/rande.html>

endpoint :: ByteString -> Maybe ByteString
endpoint "classic"        = Just "s3.amazonaws.com"
endpoint "us-east-1"      = Just "s3.amazonaws.com"
endpoint "us-west-2"      = Just "s3-us-west-2.amazonaws.com"
endpoint "us-west-1"      = Just "s3-us-west-1.amazonaws.com"
endpoint "eu-west-1"      = Just "s3-eu-west-1.amazonaws.com"
endpoint "ap-southeast-1" = Just "s3-ap-southeast-1.amazonaws.com"
endpoint "ap-northeast-1" = Just "s3-ap-northeast-1.amazonaws.com"
endpoint "sa-east-1"      = Just "s3-sa-east-1.amazonaws.com"
endpoint _                = Nothing

variables =
  ["AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION", "SSSP_BUCKET"]

fromEnv :: IO (Map ByteString ByteString)
fromEnv  = Map.fromList . catMaybes <$> mapM paired variables
 where
  paired k = ((k,) <$>) <$> maybeGetEnv k

maybeGetEnv :: ByteString -> IO (Maybe ByteString)
maybeGetEnv k = catchJust ((>> Just ()) . guard . isDoesNotExistError)
                          (Just . Bytes.pack <$> (getEnv . Bytes.unpack $ k))
                          (const (return Nothing))

fromBytes :: ByteString -> Map ByteString ByteString
fromBytes bytes = Map.fromList
  [ (k, v) | Right (k, v) <- parseOnly line <$> Bytes.lines bytes ]

line :: Parser (ByteString, ByteString)
line  = optional (string "export") *> skipSpace *> do
  (,) <$> choice (string <$> variables)
      <*> (skipWhile (inClass " :=") *> takeTill (inClass "\n\r"))

confFromContext = do
  env    <- fromEnv
  pruned <- prune <$> do
            ready <- hReady stdin
            if ready
              then  (`Map.union` env) . fromBytes <$> Bytes.getContents
              else  return env
  let bogus = [ k | k <- variables, Map.notMember k pruned ]
  if bogus /= []
    then do Bytes.hPutStrLn stderr "Missing or invalid conf parameters:"
            mapM_ (Bytes.hPutStrLn stderr . mappend "  ") bogus
            return Nothing
    else    return (Just pruned)

s3FromEnv :: IO Aws.S3Configuration
s3FromEnv = do
  res <- (endpoint =<<) <$> maybeGetEnv "AWS_REGION"
  pure $ case res of Nothing -> Aws.defaultConfiguration
                     Just s  -> Aws.defaultConfiguration{Aws.s3Endpoint=s}

allFromEnv :: IO (Aws.Configuration, Aws.S3Configuration)
allFromEnv = do
  s3cfg <- s3FromEnv
  Just creds <- Aws.loadCredentialsFromEnv
  let conf = Aws.Configuration { Aws.timeInfo = Aws.Timestamp
                               , Aws.credentials = creds
                               , Aws.logger = Aws.defaultLog Aws.Warning }
  return (conf, s3cfg)

validate :: ByteString -> ByteString -> Maybe ByteString
validate "AWS_REGION" r = endpoint r
validate _            s = guard (s /= "") >> Just s

prune :: Map ByteString ByteString -> Map ByteString ByteString
prune  = Map.mapMaybeWithKey validate
