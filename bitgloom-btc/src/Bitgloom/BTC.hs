module Bitgloom.BTC ( Availability (..)
                    , isAvailable
                    , withSession
                    , listAccounts
                    , createAddress
                    , broadcast) where

import Control.Monad.IO.Class
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad.Catch (handle)

import Text.Groom (groom)

import qualified Data.Text as T (pack)
import Data.Fixed (resolution)
import qualified Data.Vector as V
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
listAccounts client = liftIO $ do
  accounts <- Btc.listAccounts client (Just 1)

  return (HM.toList accounts)


-- | Creates new bitcoin receiving address associated with an account
createAddress :: MonadIO m
              => Btc.Client
              -> m Btc.Address
createAddress client =
  liftIO $ Btc.getNewAddress client (Just (T.pack "bitgloom"))

-- | Broadcasts a message on the blockchain in a transaction. In order to do
--   this, we will send a small amount of money to another address and include
--   a script that includes this message.
broadcast :: MonadIO m
          => Btc.Client          -- ^ Our client session
          -> Btc.Address         -- ^ Our change address
          -> Btc.BTC             -- ^ The fee we wish to pay to miners
          -> BS.ByteString       -- ^ The message to send
          -> m Btc.TransactionID -- ^ The id of the transaction that was sent to the network
broadcast client to satoshi message =
  let unspentTransactions =
        Btc.listUnspent client Nothing Nothing V.empty

      unspentBtc utxs =
        V.sum (V.map Btc.unspentAmount utxs)

      changeBtc utxs =
        unspentBtc utxs - satoshi

      transaction utxs =
        Btc.decodeRawTransaction client =<< Btc.createRawTransaction client utxs (V.singleton (to, changeBtc utxs))

      sign utxs tx =
        Btc.signRawTransaction client (Btc.decRaw tx) (Just utxs) Nothing Nothing

      send tx =
        Btc.sendRawTransaction client (Btc.rawSigned tx)

  in liftIO $ do
    utxs <- unspentTransactions

    tx   <- liftIO $ transaction utxs
    tx'  <- liftIO $ sign utxs tx

    send tx'
