{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.BTCSpec where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent (forkIO, threadDelay)

import qualified Data.Conduit as C (await)
import Data.Conduit (($$))

import qualified Data.Base32String.Default  as B32S (fromText, toBytes)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.HexString             as HS

import qualified Network.Bitcoin.Api.Client as Btc
import qualified Network.Bitcoin.Api.Mining as Btc

import Data.Word (Word16)
import           Bitgloom.BTC (  Availability (..)
                               , isAvailable
                               , advertise
                               , discover )
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing service availability" $ do
    it "should work when providing RPC host/port" $ do
      isAvailable "127.0.0.1" 18332 "user" "pass" `shouldReturn` Available
    it "should return error when providing non-existing port" $
      isAvailable "127.0.0.1" 1234  "user" "pass" `shouldReturn` ConnectionRefused
    it "should return error when providing invalid authentication info" $
      isAvailable "127.0.0.1" 18332  "nouser" "nopass" `shouldReturn` Unauthorized

  describe "when testing service" $ do
    it "can advertise a message on the blockchain" $ do
      Btc.withClient "127.0.0.1" 18332 "user" "pass" $ \client -> do

        -- Let's flush any transactions so we know for sure we have money to spend
        _   <- Btc.generate client 10

        -- Now broadcast the actual message we want to detect.
        txid <- advertise client 0.0001 header onionAddress

        result <- newEmptyMVar

        _ <- forkIO $ do
          -- Waits for a single return value
          putMVar result =<< ((discover client header) $$ C.await)

        -- Generate enough blocks for the 'discover' function to see the block
        -- that contains the transaction.
        threadDelay 1000000
        _   <- Btc.generate client 10

        takeMVar result `shouldReturn` (Just onionAddress)

        where
          header       = HS.toBytes $ HS.hexString "b76a"
          onionAddress = B32S.toBytes $ B32S.fromText "6MQF77PTMIV3BL6S"
