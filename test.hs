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


data ServiceImplementation m = (Monad m) => ServiceImplementation
  { serviceHello :: m ()
  , serviceGetLine :: m String
  , servicePutLine :: String -> m ()
  }

serviceHelloBase :: (Monad m) => ServiceImplementation m -> m ()
serviceHelloBase impl = do
    name <- serviceGetLine impl
    servicePutLine impl $ "Hello, " ++ name

realImpl :: ServiceImplementation IO
realImpl = ServiceImplementation
  { serviceHello = serviceHelloBase realImpl
  , serviceGetLine = getLine
  , servicePutLine = putStrLn
  }

mockImpl :: (Monad m, MonadReader String m, MonadWriter String m) =>
    ServiceImplementation m
mockImpl = ServiceImplementation
  { serviceHello = serviceHelloBase mockImpl
  , serviceGetLine = ask
  , servicePutLine = tell
  }

--main = serviceHello realImpl
--test = case runRWS (serviceHello mockImpl) "Dave" () of
--    (_, _, "Hello, Dave") -> True; _ -> False


doStuff :: (Monad m) => FileSystem m -> m ()
doStuff fs = do
  fsMkdir fs $ "testdir"
  
