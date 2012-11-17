import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.Process (system)
import Text.Printf (printf)
import System.Directory (createDirectory)
import System.Directory (copyFile)

hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap hashlazy . Lazy.readFile 

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"

shaman :: FilePath -> IO String
shaman path = do
  blah <- (hashFile path >>= return . toHex)
  
  -- system $ "sha1sum " ++ path
  return  blah

createDirectoryFromSha :: String -> IO String
createDirectoryFromSha (x:y:xs) = createDirectory [x,y] >> return ([x,y,'/'] ++ xs)
  
putObject :: FilePath -> IO ()
putObject filepath = do
    sha <- shaman filepath
    filebit <- createDirectoryFromSha sha
    putStrLn sha
    copyFile filepath filebit

getDirectoryFromSha :: String -> (String, String)
getDirectoryFromSha (x:y:xs) = ( [x,y], xs )
    
restoreObject :: String -> FilePath -> IO ()
restoreObject sha dest = do
    let (dir, filepath) = getDirectoryFromSha sha
    copyFile (dir ++ ['/'] ++ filepath) dest
    return ()
