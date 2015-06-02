module Bitgloom.BTC ( Availability (..)
                    , isAvailable
                    , advertise
                    , discover ) where

import           Control.Lens                                 ((^.), (&), (<>~))

import           Control.Concurrent                           (forkIO,
                                                               killThread)
import           Control.Concurrent.MVar
import           Control.Monad                                (unless)
import           Control.Monad.Catch                          (handle)
import           Control.Monad.IO.Class

import qualified Data.ByteString                              as BS
import qualified Data.Text                                    as T (Text)

import           Network.HTTP.Client                          (HttpException (..))

import qualified Data.Bitcoin.Transaction                     as Btc
import qualified Data.Bitcoin.Types                           as Btc
import qualified Data.Bitcoin.Script                          as Btc
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

-- | Broadcasts a message on the blockchain in a transaction. In our case, the
--   fee paid to broadcast a message should be configurable: it is a very important
--   aspect of the sybill resistance of Bitgloom.
--
--   A change address wil automatically be created where all the money except
--   the fee will be stored.
advertise :: MonadIO m
          => Btc.Client          -- ^ Our client session
          -> Btc.Btc             -- ^ The fee we wish to pay to miners
          -> BS.ByteString       -- ^ The message to send, might be spread over
                                 --   multiple lines.
          -> m Btc.TransactionId -- ^ The transaction that was sent to the network
advertise client fee message =
  let unspentBtc =
        foldr ((+) . (^. amount)) 0

      changeBtc utxs =
        unspentBtc utxs - fee

      -- This manpulates the transaction by adding an output script
      -- that broadcasts the message.
      addBroadcast :: Btc.Transaction -> Btc.Transaction
      addBroadcast tx =
        tx & Btc.txOut <>~ outTxs

        where
          outTxs               = [scriptify message]
          scriptify msg        = Btc.TransactionOut 0 (Btc.Script [Btc.OP_RETURN, Btc.OP_PUSHDATA msg Btc.OPCODE])

  in liftIO $ do
    utxs <- Btc.listUnspent client

    unless (unspentBtc utxs > fee)
      (ioError (userError "Not enough money to pay fee"))

    to   <- Btc.newChangeAddress client
    raw  <- Btc.create client utxs [(to, changeBtc utxs)]

    let tx = addBroadcast raw

    (tx', completed)  <- Btc.sign client tx (Just utxs) Nothing

    unless completed (error "Internal error: unable to sign transaction!")

    Btc.send client tx'

discover :: MonadIO m
         => Btc.Client    -- ^ Our client session
         -> BS.ByteString -- ^ Header that we are looking for
         -> m ()          -- ^ Conduit source which only contains only matching Transactions
