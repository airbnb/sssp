{-# LANGUAGE OverloadedStrings
  #-}
module Aws.SSSP.App where

import           Control.Applicative
import qualified Data.ByteString.Char8 as Bytes
import           Data.Monoid
import           System.Environment
import           System.Exit
import           System.IO

import qualified Network.Wai.Handler.Warp as WWW
import qualified Network.Wai.Middleware.RequestLogger as WWW

import Aws.SSSP.Configuration
import Aws.SSSP


app = args =<< getArgs

args [     ] = web
args ["web"] = web
args args    = argumentError args

web = do
  res <- conf
  case res of Left map         -> err (misconfigured map)
              Right (ctx, www) -> WWW.runSettings www (WWW.logStdout (wai ctx))
 where
  misconfigured map = mappend "!!! Misconfigured for web; please check:\n"
                              (render map)

argumentError args = (err . Bytes.unlines)
  ("Not able to interpret these command line arguments:" : (fmt <$> args))
 where
  fmt = (mappend "  ") . Bytes.pack

msg s = Bytes.hPutStrLn stderr s
err s = msg s >> exitFailure 

