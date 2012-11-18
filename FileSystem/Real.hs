module FileSystem.Real(realFs) where

import qualified Data.ByteString.Lazy as Lazy
import System.Directory
import FileSystem


realFs :: FileSystem IO
realFs = FileSystem
  { fsLazyRead = Lazy.readFile
  , fsCopy     = copyFile
  , fsMkdir    = createDirectoryIfMissing True
  }

