{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes #-}

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.Process (system)
import Text.Printf (printf)
import Control.Monad
import Control.Monad.RWS

import FileSystem
import FileSystem.Real
import FileSystem.Fake as F
import ObjectStore


doStuff :: (Monad m) => FileSystem m -> m ()
doStuff fs = do
  fsMkdir fs "testdir"
  fsCopy fs "test.hs" "testdir/x.hs"

  
