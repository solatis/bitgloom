module Bitgloom.BTC ( Availability (..)
                    , isAvailable
                    , listAccounts
                    , createAddress
                    , broadcast) where

import           Control.Lens                                 ((^.))

import           Control.Concurrent                           (forkIO,
                                                               killThread)
import           Control.Concurrent.MVar
import           Control.Monad                                (unless)
import           Control.Monad.Catch                          (handle)
import           Control.Monad.IO.Class

import qualified Data.ByteString                              as BS
import qualified Data.Text                                    as T (Text, pack)

import           Network.HTTP.Client                          (HttpException (..))

import qualified Data.Bitcoin.Transaction                     as Btc ()
import qualified Data.Bitcoin.Types                           as Btc
import qualified Network.Bitcoin.Api.Client                   as Btc
import qualified Network.Bitcoin.Api.Misc                     as Btc
import qualified Network.Bitcoin.Api.Transaction              as Btc
import           Network.Bitcoin.Api.Types.UnspentTransaction (amount)
import qualified Network.Bitcoin.Api.Wallet                   as Btc

data Availability =
  Available |
  ConnectionRefused |
  IncorrectPort |
  Unauthorized
  deriving (Show, Eq)

isAvailable :: MonadIO m => String -> Int -> T.Text -> T.Text -> m Availability
isAvailable host port user pass =
  let performTest errMsg = do
        _ <- Btc.withClient host port user pass Btc.getInfo
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

listAccounts :: MonadIO m
             => Btc.Client                 -- ^ Our client session
             -> m [(Btc.Account, Btc.Btc)] -- ^ A list with all accounts and their associated BTC
listAccounts client = liftIO $
  Btc.listAccounts client


-- | Creates new bitcoin receiving address associated with an account
createAddress :: MonadIO m
              => Btc.Client
              -> m Btc.Address
createAddress client =
  liftIO $ Btc.newAddressWith client (T.pack "bitgloom")

-- | Broadcasts a message on the blockchain in a transaction. In our case, the
--   fee paid to broadcast a message should be configurable: it is a very important
--   aspect of the sybill resistance of Bitgloom.
--
--   A change address wil automatically be created where all the money except
--   the fee will be stored.
broadcast :: MonadIO m
          => Btc.Client          -- ^ Our client session
          -> Btc.Btc             -- ^ The fee we wish to pay to miners
          -> BS.ByteString       -- ^ The message to send
          -> m Btc.TransactionId -- ^ The transaction that was sent to the network
broadcast client fee message =
  let unspentBtc =
        foldr ((+) . (^. amount)) 0

      changeBtc utxs =
        unspentBtc utxs - fee

  in liftIO $ do
    utxs <- liftIO $ Btc.listUnspent client

    unless (unspentBtc utxs > fee)
      (ioError (userError "Not enough money to pay fee"))

    to   <- liftIO $ Btc.newChangeAddress client
    tx   <- liftIO $ Btc.create client utxs [(to, changeBtc utxs)]

    putStrLn ("decoded = " ++ show tx)

    (tx', completed)  <- liftIO $ Btc.sign client tx (Just utxs) Nothing

    unless completed (error "Internal error: unable to sign transaction!")

    Btc.send client tx'
