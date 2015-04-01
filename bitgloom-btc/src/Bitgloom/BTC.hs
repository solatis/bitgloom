module Bitgloom.BTC where

import Control.Monad.IO.Class
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad.Catch (handle)

import qualified Data.ByteString as BS

import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Types (Status)

import Network.Bitcoin ( getClient
                       , getBitcoindInfo)

data Availability =
  Available |
  ConnectionRefused |
  IncorrectPort |
  Unauthorized
  deriving (Show, Eq)

isAvailable :: MonadIO m => String -> Int -> BS.ByteString -> BS.ByteString -> m Availability
isAvailable host port user pass =
  let performTest errMsg = do
        client <- getClient ("http://" ++ host ++ ":" ++ show port) user pass
        _      <- getBitcoindInfo client
        putMVar errMsg Available

      handleExceptions errMsg (FailedConnectionException2 {}) = putMVar errMsg ConnectionRefused
      handleExceptions errMsg (StatusCodeException {}) = putMVar errMsg Unauthorized

  in do
    errMsg <- liftIO newEmptyMVar

    threadId <- liftIO $ forkIO $
      handle
        (handleExceptions errMsg)
        (performTest errMsg)

    res <- liftIO $ takeMVar errMsg
    liftIO $ killThread threadId

    return res
