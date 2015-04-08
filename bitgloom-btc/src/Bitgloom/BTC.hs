module Bitgloom.BTC ( Availability (..)
                    , isAvailable
                    , withSession
                    , listAccounts) where

import Control.Monad.IO.Class
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad.Catch (handle)

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Types (Status)

import qualified Network.Bitcoin as Btc

data Availability =
  Available |
  ConnectionRefused |
  IncorrectPort |
  Unauthorized
  deriving (Show, Eq)

buildClientUri :: String -> Int -> String
buildClientUri host port = "http://" ++ host ++ ":" ++ show port

isAvailable :: MonadIO m => String -> Int -> BS.ByteString -> BS.ByteString -> m Availability
isAvailable host port user pass =
  let performTest errMsg = do
        client <- Btc.getClient (buildClientUri host port) user pass
        _      <- Btc.getBitcoindInfo client
        putMVar errMsg Available

      handleExceptions errMsg (FailedConnectionException2 {}) = putMVar errMsg ConnectionRefused
      handleExceptions errMsg (StatusCodeException {}) = putMVar errMsg Unauthorized
      handleExceptions errMsg (InvalidStatusLine {}) = putMVar errMsg IncorrectPort

  in do
    errMsg <- liftIO newEmptyMVar

    threadId <- liftIO $ forkIO $
      handle
        (handleExceptions errMsg)
        (performTest errMsg)

    res <- liftIO $ takeMVar errMsg
    liftIO $ killThread threadId

    return res

withSession :: MonadIO m
            => String              -- ^ Hostname
            -> Int                 -- ^ Port
            -> BS.ByteString       -- ^ Username
            -> BS.ByteString       -- ^ Password
            -> (Btc.Client -> m a) -- ^ Computation to run
            -> m a                 -- ^ Our Btc client session
withSession host port user pass callback = do
  client <- liftIO $ Btc.getClient (buildClientUri host port) user pass
  callback client

listAccounts :: MonadIO m
             => Btc.Client                 -- ^ Our client session
             -> m [(Btc.Account, Btc.BTC)] -- ^ A list with all accounts and their associated BTC
listAccounts client = do
  accounts <- liftIO $ Btc.listAccounts client (Just 1)

  return (HM.toList accounts)
