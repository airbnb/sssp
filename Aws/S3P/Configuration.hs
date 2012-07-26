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
import           System.Environment
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

[access,secret,region,bucket] =
  ["AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION", "SSSP_BUCKET"]

maybeGetEnv :: ByteString -> IO (Maybe ByteString)
maybeGetEnv k = catchJust ((>> Just ()) . guard . isDoesNotExistError)
                          (Just . Bytes.pack <$> (getEnv . Bytes.unpack $ k))
                          (const (return Nothing))

fromEnv = sequence [ maybeGetEnv access
                   , maybeGetEnv secret
                   , join . (endpoint <$>) <$> maybeGetEnv region
                   , maybeGetEnv bucket ]

parseLine =
  optional (string "export") *> skipSpace *> do
    k <- choice (string <$> [access,secret,region,bucket]) 
    v <- skipWhile (inClass " :=") *> takeByteString
    return . (k,) $
      if k == region then endpoint v else guard (v /= "") >> Just v 


s3FromEnv :: IO Aws.S3Configuration
s3FromEnv = do
  res <- join . (endpoint <$>) <$> maybeGetEnv "AWS_REGION"
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

