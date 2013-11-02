{-# LANGUAGE OverloadedStrings #-}

module DataLog (
  DataLog,
  withDataLog,
  writeToLog
) where

import Data.Aeson
import Control.Concurrent
import Control.Exception
import System.IO
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy as BSL
import Data.Text as T

import Util

data DataLog = DataLog {
    mutex :: MVar ()
  , startµs :: Integer
  , file :: Handle
}

withDataLog :: FilePath -> (DataLog -> IO r) -> IO r
withDataLog path f = withFile path WriteMode perform
  where
    perform h = do
      BSC.hPutStr h "[{\"version\":\"1\"}"
      m <- newMVar ()
      s <- currentTime
      result <- f $ DataLog { mutex = m, startµs = s, file = h }
      BSC.hPutStrLn h "]"
      return result

comma :: BS.ByteString
comma = ","

writeToLog :: (ToJSON a) => DataLog -> a -> IO ()
writeToLog datalog msg =
  do
    me <- myThreadId
    now <- currentTime
    let bytes = encode $ object [
          "thread" .= T.pack (show me)
          , "at" .= (now - startµs datalog)
          , "msg" .= toJSON msg ]
    bracket_ (takeMVar $ mutex datalog) (putMVar (mutex datalog) ()) (writeLine bytes)
  where
    writeLine :: BSL.ByteString -> IO ()
    writeLine bytes = do
      BS.hPutStr (file datalog) comma
      BSL.hPutStr (file datalog) bytes
