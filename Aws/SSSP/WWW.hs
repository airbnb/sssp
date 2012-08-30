-- | Helper functions for writing the web server.
module Aws.SSSP.WWW where

import           Data.ByteString (ByteString)

import qualified Blaze.ByteString.Builder as Blaze
import           Control.Monad.Trans
import           Data.Conduit (($=))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Network.HTTP.Conduit as Conduit
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

