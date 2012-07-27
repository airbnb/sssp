#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings
  #-}
import qualified Data.ByteString.Char8 as Bytes
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           System.Exit
import           System.IO

import qualified Network.Wai.Handler.Warp as WWW

import Aws.SSSP.Configuration
import Aws.SSSP


main = do
  res <- conf
  case res of Left map  -> err (misconfigured map)
              Right (ctx, www) -> WWW.runSettings www (wai ctx)
 where
  misconfigured map = mappend "!!! Misconfigured; please check:\n" (render map)

msg s = Bytes.hPutStrLn stderr s
err s = msg s >> exitFailure 

