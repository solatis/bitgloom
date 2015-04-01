{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.BTCSpec where

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
