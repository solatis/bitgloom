module Bitgloom.BTC ( Availability (..)
                    , isAvailable
                    , listAccounts
                    , createAddress
                    , broadcast) where

import           Control.Lens                                 ((^.))

import           Control.Concurrent                           (forkIO,
                                                               killThread)
import           Control.Concurrent.MVar
import           Control.Monad.Catch                          (handle)
import           Control.Monad.IO.Class

import           Text.Groom                                   (groom)

import qualified Data.ByteString                              as BS
import qualified Data.Text                                    as T (Text,
                                                                    pack)

import           Network.HTTP.Client                          (HttpException (..))

import qualified Data.Bitcoin.Transaction                     as Btc ()
import qualified Data.Bitcoin.Types                           as Btc
import qualified Network.Bitcoin.Api.Client                   as Btc
import qualified Network.Bitcoin.Api.Misc                     as Btc
import qualified Network.Bitcoin.Api.Transaction              as Btc
import qualified Network.Bitcoin.Api.Wallet                   as Btc
import           Network.Bitcoin.Api.Types.UnspentTransaction (amount)

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

-- | Broadcasts a message on the blockchain in a transaction. In order to do
--   this, we will send a small amount of money to another address and include
--   a script that includes this message.
broadcast :: MonadIO m
          => Btc.Client          -- ^ Our client session
          -> Btc.Address         -- ^ Our change address
          -> Btc.Btc             -- ^ The fee we wish to pay to miners
          -> BS.ByteString       -- ^ The message to send
          -> m Btc.TransactionId -- ^ The transaction that was sent to the network
broadcast client to satoshi message =
  let unspentBtc =
        foldr ((+) . (^. amount)) 0

      changeBtc utxs =
        unspentBtc utxs - satoshi

      transaction utxs =
        Btc.create client utxs [(to, changeBtc utxs)]

      sign utxs tx =
        Btc.sign client tx (Just utxs) Nothing

  in liftIO $ do
    utxs <- liftIO $ Btc.listUnspent client
    tx   <- liftIO $ transaction utxs

    putStrLn ("decoded = " ++ groom tx)

    (tx', completed)  <- liftIO $ sign utxs tx

    Btc.send client tx'
