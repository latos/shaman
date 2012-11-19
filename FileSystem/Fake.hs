module FileSystem.Fake(
  fakeFs, 
  FakeFs(..), 
  FsEnt(..), 
  module Control.Monad.State,
  fromList, 
  prefixes,
  splitPath,
  dirname,
  readFile,
  writeFile,
  writeFile',
  mkdir) where

import Prelude hiding (writeFile, readFile)

import qualified Data.ByteString.Lazy as Lazy
import FileSystem
import Control.Monad.State
import qualified Data.Map as M
import Data.List(partition)
import Data.Map((!))
import Data.List.Split
import Control.Exception
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.Bifunctor as B

data FsEnt = File { content :: Lazy.ByteString } | Dir (M.Map String FsEnt)
  deriving (Eq)

instance Show FsEnt where
  show e = foldl (joiner "\n") "== FsEnt ==" $ showEntry "" e

showEntry :: String -> FsEnt -> [String]
showEntry prefix (File c) = [prefix ++ " " ++ show (U.toString c)]
showEntry prefix (Dir m) = (prefix' :)
  $ map (prefix' ++) 
  $ M.toList m >>= uncurry showEntry
  where prefix' = prefix ++ "/"

type FakeFs = FsEnt

type FsState = State FakeFs

empty = Dir M.empty

assert' t msg v
  | t         = v
  | otherwise = error msg

assertF' f msg v = assert' (f v) msg v

splitPath p = id
  $ assertF' (all (/= "") . tail) ("Empty path component in " ++ show p)
  $ assertF' ((/= "") . last)     ("Trailing '/' in " ++ show p)
  $ assertF' ((/= "") . head)     ("Leading '/' in " ++ show p)
  $ splitOn "/" p


joiner m a b = a ++ m ++ b

dirname path = foldl1 (joiner "/") (init $ splitPath path)

-- | takes a path and returns all prefixes
-- e.g. "/a/b/c" -> ["/a", "/a/b", "/a/b/c"]
prefixes :: FilePath -> [FilePath]
prefixes = tail . scanl1 (joiner "/") . splitPath

fromList :: [(FilePath, String)] -> FakeFs
fromList entries = dirsAndFiles
  where
    justDirs     = foldr mkdir empty dirs
    dirsAndFiles = foldr (\(p, c) -> writeFile' c p) justDirs files
    
    dirs = map (init . fst) dirs' ++ (map dirname . filter (any (=='/')) . map fst) files
    (dirs', files) = partition ((== '/') . last . fst) entries

copyFile :: FilePath -> FilePath -> FakeFs -> FakeFs
copyFile from to fs = writeFile (readFile from fs) to fs

readFile :: FilePath -> FakeFs -> Lazy.ByteString
readFile fullPath = read' (splitPath fullPath)
  where
    read' [] (File c) = c
    read' [] (Dir  _) = err "is a directory"
    read' (n:ns) (File _) = err "does not exist"
    read' (n:ns) (Dir  m) = case M.lookup n m of
      Just sub -> read' ns sub
      Nothing  -> err "does not exist"
    err = mkErr "readFile" fullPath

writeFile :: Lazy.ByteString -> FilePath -> FakeFs -> FakeFs
writeFile content fullPath = write' (splitPath fullPath)
  where
    write' [] (File _) = File content
    write' [] (Dir  _) = err "is a directory"
    write' [n] (Dir m) = Dir (M.insert n (File content) m)
    write' (n:ns) (File _) = err "does not exist"
    write' (n:ns) (Dir  m) = case M.lookup n m of
      Just sub -> Dir (M.insert n (write' ns sub) m)
      Nothing  -> err "does not exist"
    err = mkErr "writeFile" fullPath

mkdir :: FilePath -> FakeFs -> FakeFs
mkdir fullPath = mkdir' (splitPath fullPath)
  where
    mkdir' [] (File c) = err "is a file"
    mkdir' [] d        = d
    mkdir' (n:ns) (File _) = err "subpath is file"
    mkdir' (n:ns) (Dir  m) = Dir (M.insert n (mkdir' ns (M.findWithDefault empty n m)) m)
    err = mkErr "mkdir" fullPath

mkErr fn p = error . ((fn ++ ": " ++ show p ++ " ") ++)

-- | Convenience alternative that takes a String as the file contents.
writeFile' :: String -> FilePath -> FakeFs -> FakeFs
writeFile' = writeFile . U.fromString


-- compose with the 2nd
c2 f g a = f . g a

fakeFs :: FileSystem FsState
fakeFs = FileSystem
  { fsRead = gets . readFile
  , fsCopy     = modify `c2` copyFile
  , fsMkdir    = modify . mkdir
  }
