{-# LANGUAGE OverloadedStrings
           , TupleSections #-}
-- | Utilities for determining the server configuration from environment
--   variables and file input.
module Aws.SSSP.Configuration where

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
import qualified Data.Conduit.Network as Conduit
import           Data.Default
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Network.HTTP.Conduit as Conduit
import qualified Network.Wai.Handler.Warp as WWW

import Aws.SSSP (Ctx(..))


variables =
  [ "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION", "SSSP_BUCKET",
    "SSSP_CONN", "PORT" ]

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
            <*> (skipWhile (inClass " :=") *> chopped)
 where
  chopped = fst . Bytes.spanEnd isSpace <$> takeTill (inClass "\n\r")

fromEnvAndSTDIN = do
  env    <- fromEnv
  prune <$> do go <- checkForInput
               if go then (`Map.union` env) . fromBytes <$> Bytes.getContents
                     else return env
 where
  checkForInput = catchJust ((>> Just ()) . guard . isEOFError)
                            (hReady stdin)
                            (const (return False))


conf :: IO (Either (Map ByteString ByteString) (Ctx, WWW.Settings))
conf  = do
  map  <- fromEnvAndSTDIN
  ctx' <- createCtx map
  return $ maybe (Left map) Right ((,) <$> ctx' <*> createSettings map)

createSettings :: Map ByteString ByteString -> Maybe WWW.Settings
createSettings map = do
  (host,port) <- (splitConn =<< read "SSSP_CONN") <|>
                 ("127.0.0.1",) <$> (read "PORT" <|> Just "8000")
  host        <- parseHost host
  port        <- parsePort port
  Just WWW.defaultSettings{WWW.settingsPort=port,WWW.settingsHost=host}
 where
  read k       = Map.lookup k map
  splitConn bs = case Bytes.split ':' bs of [host,port] -> Just (host, port)
                                            _           -> Nothing
  parsePort :: ByteString -> Maybe Int
  parsePort      = (fst <$>) . listToMaybe . reads . Bytes.unpack
  parseHost :: ByteString -> Maybe Conduit.HostPreference
  parseHost "*"  = Just Conduit.HostAny
  parseHost "*4" = Just Conduit.HostIPv4
  parseHost "*6" = Just Conduit.HostIPv6
  parseHost bs   = Just (Conduit.Host (Bytes.unpack bs))

createCtx :: Map ByteString ByteString -> IO (Maybe Ctx)
createCtx map = do
  manager <- Conduit.newManager def
  return $ do
    aws    <- aws <$> read "AWS_ACCESS_KEY_ID" <*> read "AWS_SECRET_ACCESS_KEY"
    s3     <- s3Configured <|> Just Aws.defaultConfiguration
    bucket <- utf8 <$> read "SSSP_BUCKET"
    Just Ctx{bucket=bucket, aws=aws, s3=s3{Aws.s3UseUri=True}, manager=manager}
 where
  read k       = Map.lookup k map
  aws id key   = Aws.Configuration { Aws.timeInfo = Aws.Timestamp
                                   , Aws.credentials = Aws.Credentials id key
                                   , Aws.logger = Aws.defaultLog Aws.Warning }
  s3Configured = do region <- read "AWS_REGION"
                    url    <- endpoint region
                    Just Aws.defaultConfiguration{Aws.s3Endpoint=url}
  utf8 = Text.decodeUtf8With Text.lenientDecode

validate :: ByteString -> ByteString -> Maybe ByteString
validate "AWS_REGION" r = endpoint r
validate _            s = guard (s /= "") >> Just s

prune :: Map ByteString ByteString -> Map ByteString ByteString
prune  = Map.mapMaybeWithKey validate

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

render :: Map ByteString ByteString -> ByteString
render  = Bytes.unlines . (kv <$>) . Map.toAscList
 where
  kv (k, v) = mconcat [k, ": ", v]

