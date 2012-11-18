module FileSystem.Fake(fakeFs, new, FakeFs(..), FsEnt(..), runState) where

import Prelude hiding (writeFile, readFile)

import qualified Data.ByteString.Lazy as Lazy
import FileSystem
import Control.Monad.State
import qualified Data.Map as M
import Data.Map((!))
import Data.List.Split
import Control.Exception
import qualified Data.ByteString.Lazy.UTF8 as U

data FsEnt = File { content :: Lazy.ByteString } | Dir
  deriving (Eq, Show)

data FakeFs = FakeFs { entries :: M.Map String FsEnt }
  deriving (Eq, Show)

--new :: FsState
--new = state $ FakeFs (M.singleton "" Dir)

type FsState = State FakeFs

splitPath p = assert (head p == '/') $ splitOn "/" p

joiner a b = a ++ "/" ++ b

dirname path = foldl1 joiner (init $ splitPath path)

-- | takes a path and returns all prefixes
-- e.g. "a/b/c" -> ["a", "a/b", "a/b/c"]
prefixes :: FilePath -> [FilePath]
prefixes = scanl1 joiner . splitPath

assertPath :: FilePath -> FakeFs -> FakeFs
assertPath path fs
  | all (flip M.member $ entries fs) (prefixes path) = fs
  | otherwise = error $ "Path does not exist: '" ++ path ++ "'"


--fromList :: [(String, String)] -> FakeFs
--fromList 

writeFile :: FilePath -> Lazy.ByteString -> FakeFs -> FakeFs
--writeFile path content fs = FakeFs $ M.insert path (File $ U.fromString content) m
writeFile path content fs = FakeFs $ M.insert path (File content) m
  where m = entries $ assertPath (dirname path) fs

--
--lazyRead :: FilePath -> FsState Lazy.ByteString
--lazyRead fp = do
--  fs <- get
--  return $ content $ (entries fs) ! fp

--writeFile :: FilePath -> String -> FsState ()
--writeFile fp content = modify $ \s ->

copyFile :: FilePath -> FilePath -> FakeFs -> FakeFs
copyFile from to fs = writeFile to (readFile from fs) fs

readFile :: FilePath -> FakeFs -> Lazy.ByteString
readFile path fs = content $ (entries fs) ! path

-- compose with the 2nd
c2 f g a = f . g a

fakeFs :: FileSystem FsState
fakeFs = FileSystem
  { fsLazyRead = gets . readFile
  , fsCopy     = modify `c2` copyFile
  , fsMkdir    = error "hi"
  }
