{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses #-}
-- | Use environment variables and config information to determine how to
--   connect to AWS services.
module Aws.S3P.AwsConnection where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import System.Environment
import System.IO.Error

import qualified Aws as Aws
import qualified Aws.S3 as Aws


-- | Interpret a region name, like @us-west-1@, in accord with the Amazon's
--   documentation for endpoint lcoations.
--   <http://docs.amazonwebservices.com/general/latest/gr/rande.html>

endpoint :: String -> Maybe ByteString
endpoint "classic"        = Just "s3.amazonaws.com"
endpoint "us-east-1"      = Just "s3.amazonaws.com"
endpoint "us-west-2"      = Just "s3-us-west-2.amazonaws.com"
endpoint "us-west-1"      = Just "s3-us-west-1.amazonaws.com"
endpoint "eu-west-1"      = Just "s3-eu-west-1.amazonaws.com"
endpoint "ap-southeast-1" = Just "s3-ap-southeast-1.amazonaws.com"
endpoint "ap-northeast-1" = Just "s3-ap-northeast-1.amazonaws.com"
endpoint "sa-east-1"      = Just "s3-sa-east-1.amazonaws.com"
endpoint _                = Nothing

s3FromEnv :: IO Aws.S3Configuration
s3FromEnv = do
  res <- catchJust (\e -> guard (isDoesNotExistError e) >> Just ())
                   (endpoint <$> System.Environment.getEnv "AWS_REGION")
                   (\_ -> pure Nothing)
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

