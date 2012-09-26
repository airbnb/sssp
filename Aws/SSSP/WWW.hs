{-# LANGUAGE OverloadedStrings
  #-}
-- | Helper functions for writing the web server.
module Aws.SSSP.WWW where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Maybe

import qualified Aws.S3 as Aws
import qualified Blaze.ByteString.Builder as Blaze
import           Control.Monad.Trans
import           Crypto.Hash.MD5 (MD5)
import qualified Crypto.Hash.MD5 as MD5
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
import           Data.Conduit (($=))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.List as List
import qualified Data.Serialize as Ser
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Network.HTTP.Conduit as Conduit
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai


proxied :: Conduit.Manager -> String -> Conduit.ResourceT IO Wai.Response
proxied manager string        = do
  request                    <- liftIO $ Conduit.parseUrl string
  Conduit.Response s _ h src <- Conduit.http request manager
  src                        <- reSource src
  return $ Wai.ResponseSource s h (src $= b2b)

reSource :: MonadIO m => Conduit.ResumableSource m o -> m (Conduit.Source m o)
reSource resumable     = do
  (src, finalizer)    <- Conduit.unwrapResumable resumable
  return $ Conduit.addCleanup (const finalizer) src

b2b :: (Monad m) => Conduit.Conduit ByteString m (Conduit.Flush Blaze.Builder)
b2b  = Conduit.map (Conduit.Chunk . Blaze.fromByteString)

addHeaders :: Aws.PutObject -> HTTP.RequestHeaders -> Aws.PutObject
addHeaders = List.foldl' add
 where
  add :: Aws.PutObject -> HTTP.Header -> Aws.PutObject
  add po (k, v)
    | "Content-Type" == k        = po{ Aws.poContentType        = Just v }
    | "Cache-Control" == k       = po{ Aws.poCacheControl       = Just (t v) }
    | "Content-Disposition" == k = po{ Aws.poContentDisposition = Just (t v) }
    | "Content-Encoding" == k    = po{ Aws.poContentEncoding    = Just (t v) }
    | "Content-MD5" == k         = po{ Aws.poContentMD5         = unMD5 v }
    | "Expires" == k             = po{ Aws.poExpires            = Just (i v) }
    | "x-amz-acl" == k           = po{ Aws.poAcl                = acl v }
    | "x-amz-storage-class" == k = po{ Aws.poStorageClass       = storage v }
    | amzK /= b                  = po{ Aws.poMetadata           = newMeta }
    | otherwise                  = po
   where
    newMeta = (t amzK, t v) : Aws.poMetadata po
    b = CI.original k
    amzK | amzMeta `Bytes.isPrefixOf` b = Bytes.drop (Bytes.length amzMeta) b
         | otherwise                    = b
    amzMeta = "x-amz-meta-"
    t = Text.decodeUtf8With Text.ignore
    i  = maybe 0 fst . listToMaybe . reads . Bytes.unpack
    acl "private"                   = Just Aws.AclPrivate
    acl "public-read"               = Just Aws.AclPublicRead
    acl "public-read-write"         = Just Aws.AclPublicReadWrite
    acl "authenticated-read"        = Just Aws.AclAuthenticatedRead
    acl "bucket-owner-read"         = Just Aws.AclBucketOwnerRead
    acl "bucket-owner-full-control" = Just Aws.AclBucketOwnerFullControl
    acl "log-delivery-write"        = Just Aws.AclLogDeliveryWrite
    acl _                           = Nothing
    storage "STANDARD"           = Just Aws.Standard
    storage "REDUCED_REDUNDANCY" = Just Aws.ReducedRedundancy
    storage _                    = Nothing
    unMD5 b = either (const Nothing) Just (Ser.decode =<< Base64.decode b)

