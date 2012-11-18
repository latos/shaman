module FileSystem.Fake(
  fakeFs, 
  FakeFs(..), 
  FsEnt(..), 
  runState, 
  fromList, 
  dirFlag) where

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

data FakeFs = FakeFs { entries :: M.Map FilePath FsEnt }
  deriving (Eq)

instance Show FakeFs where
  show (FakeFs entries) = foldl (joiner "\n") "FakeFs" $ 
    map (uncurry showEntry) (M.toList entries)
    where
      showEntry "" Dir = "d /"
      showEntry k Dir = "d " ++ k
      showEntry k (File c) = "f " ++ k ++ " " ++ show (U.toString c)

--new :: FsState
--new = state $ FakeFs (M.singleton "" Dir)

type FsState = State FakeFs


assert' t msg v
  | t         = v
  | otherwise = error msg

assertF' f msg v = assert' (f v) msg v

splitPath p = id
  $ assertF' (all (/= "") . tail) ("Empty path component in " ++ show p)
  $ assertF' ((/= "") . last)     ("Trailing '/' in " ++ show p)
  $ assertF' ((== "") . head)     ("Path must start with '/' got " ++ show p)
  $ assert'  (p /= "") ("Empty path")
  $ splitOn "/" p


joiner m a b = a ++ m ++ b

dirname path = foldl1 (joiner "/") (init $ splitPath path)

-- | takes a path and returns all prefixes
-- e.g. "a/b/c" -> ["a", "a/b", "a/b/c"]
prefixes :: FilePath -> [FilePath]
prefixes = scanl1 (joiner "/") . splitPath

assertPath :: FilePath -> FakeFs -> FakeFs
assertPath path fs
  | all (flip M.member $ entries fs) (prefixes path) = fs
  | otherwise = error $ "Path does not exist: '" ++ path ++ "'"


dirFlag = "<D>"

mapSnd f (a, b) = (a, f b)

fromList :: [(FilePath, String)] -> FakeFs
fromList entries = FakeFs . M.fromList $ 
  -- expand all paths and treat as dirs
  (map (\p -> (p, Dir)) expandedPaths) ++
  -- clobber any non-dir entries with file entries
  (map (mapSnd (File . U.fromString)) (filter ((/= dirFlag) . snd) entries))
  where
    expandedPaths = fst (unzip entries) >>= prefixes

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
