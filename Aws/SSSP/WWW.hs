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
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.CaseInsensitive as CI
import           Data.Conduit (($=))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.List as List
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
  add po (k, v)
    | "Content-Type" == k        = po{ Aws.poContentType        = Just v }
    | "Cache-Control" == k       = po{ Aws.poCacheControl       = Just (t v) }
    | "Content-Disposition" == k = po{ Aws.poContentDisposition = Just (t v) }
    | "Content-Encoding" == k    = po{ Aws.poContentEncoding    = Just (t v) }
 -- | "Content-MD5" == k         = po{ Aws.poContentMD5         = Just (m v) }
    | "Expires" == k             = po{ Aws.poExpires            = Just (i v) }
 -- | "x-amz-acl" == k           = po{ Aws.poAcl                = Just (t v) }
 -- | "x-amz-storage-class" == k = po{ Aws.poStorageClass       = Just (t v) }
 -- | clip k /= k                = po{ ... }
    | otherwise                  = po
  -- TODO: Handle extended headers.
--clip | "x-amz-" `Bytes.isPrefixOf` s = Bytes.drop (Bytes.length "x-amz-") k
--     | otherwise                     = k
  t = Text.decodeUtf8With Text.ignore
--t' (k, v) = (t (CI.original k), t v)
  i  = maybe 0 fst . listToMaybe . reads . Bytes.unpack

