{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.BTCSpec where

import qualified Data.Base32String.Default  as B32S (fromText, toBytes)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.HexString             as HS
import Data.Monoid ((<>))

import qualified Network.Bitcoin.Api.Client as Btc
import qualified Network.Bitcoin.Api.Mining as Btc

import Data.Word (Word16)
import           Bitgloom.BTC
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
    it "should return error when providing the wrong port" $
      isAvailable "127.0.0.1" 7656  "user" "pass" `shouldReturn` IncorrectPort

  describe "when testing service" $ do
    it "can advertise a message on the blockchain" $ do
      Btc.withClient "127.0.0.1" 18332 "user" "pass" $ \client -> do

        -- Let's flush any transactions so we know for sure we have money to spend
        _   <- Btc.generate client 1
        txid <- advertise client 0.0001 message
        putStrLn ("txid = " ++ show txid)
        True `shouldBe` True

        where
          header       = HS.toBytes $ HS.hexString "b76a"
          onionAddress = B32S.toBytes $ B32S.fromText "6MQF77PTMIV3BL6S"

          message      = header <> onionAddress
