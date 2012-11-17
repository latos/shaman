import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import System.Process (system)
import Text.Printf (printf)
import Control.Monad
import Control.Monad.State

hashFile :: FilePath -> IO Strict.ByteString
--hashFile = fmap hashlazy . Lazy.readFile 
--hashFile x = Lazy.readFile x >>= return . hashlazy
hashFile = xx hashlazy . Lazy.readFile 

xx f a = (>>=) a (return . f)

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"

test :: FilePath -> IO ()
test path = do
  hashFile path >>= putStrLn . toHex
  system $ "sha1sum " ++ path
  return ()




data Foo m a = (Monad m) => Foo (m a)
  
data ObjectStore m = (Monad m) => ObjectStore {
  putObj :: FilePath -> m (),
  getObj :: String -> FilePath -> m ()
  }



--x = State
