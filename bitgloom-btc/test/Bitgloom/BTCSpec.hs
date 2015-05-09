{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.BTCSpec where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Base58String as B58S (toText)

import qualified Network.Bitcoin.Api.Client                   as Btc
import qualified Network.Bitcoin.Api.Mining                   as Btc

import Bitgloom.BTC
import Test.Hspec

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

  describe "when creating a session" $ do
    it "can look up accounts" $ do
      result <- Btc.withClient "127.0.0.1" 18332 "user" "pass" listAccounts
      length (result) `shouldSatisfy` (> 0)

    it "can create a new address" $ do
      address <- Btc.withClient "127.0.0.1" 18332 "user" "pass" createAddress

      T.length (B58S.toText address) `shouldSatisfy` (>= 27)
      T.length (B58S.toText address) `shouldSatisfy` (<= 34)

    it "can broadcast a message to an address" $ do
      Btc.withClient "127.0.0.1" 18332 "user" "pass" $ \client -> do
        -- Let's flush any transactions so we know for sure we have money to spend
        _ <- Btc.generate client 10
        _ <- broadcast client 0.0001 (TE.encodeUtf8 (T.pack "hello world"))
        True `shouldBe` True
