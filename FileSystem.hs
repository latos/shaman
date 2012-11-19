{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes #-}

module FileSystem (FileSystem(..)) where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Control.Monad.RWS

data FileSystem m = (Monad m) => FileSystem
  { fsRead :: FilePath -> m Lazy.ByteString
  , fsCopy     :: FilePath -> FilePath -> m ()
  , fsMkdir    :: FilePath -> m ()
  }


