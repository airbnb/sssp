{-# LANGUAGE OverloadedStrings
           , ParallelListComp
           , RecordWildCards
           , StandaloneDeriving
           , PatternGuards
  #-}

import           Control.Applicative
import           Control.Monad
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
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Data.Attempt
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import           Data.Conduit (($$+-))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Network.Wai as Wai
import qualified Network.HTTP.Conduit as Conduit
import qualified Network.HTTP.Types as HTTP

-- wai :: String -> WWW.Application
-- wai s req@WWW.Request{..} = Conduit.withManager go
--  where
--   go manager                    = do
--     request                    <- liftIO $ Conduit.parseUrl s
--     Conduit.Response s _ h src <- Conduit.http request manager
--     return $ WWW.ResponseSource s h src'

-- resumeableToResponse
--  :: (Monad m)
--  => Sink ByteString m (Conduit.Source m (Conduit.Flush Blaze.Builder))
-- resumeableToResponse = Conduit.map (Conduit.Chunk . Blaze.fromByteString)


b2b :: (Monad m) => Conduit.Conduit ByteString m (Conduit.Flush Blaze.Builder)
b2b  = Conduit.map (Conduit.Chunk . Blaze.fromByteString)

webResponder :: Conduit.Sink (Conduit.Chunk Blaze.Builder)
                             (Conduit.ResourceT IO) Wai.Response
webResponder = do



