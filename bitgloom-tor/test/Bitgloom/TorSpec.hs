{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.I2PSpec where

import Bitgloom.I2P
import Test.Hspec

spec :: Spec
spec = do
  describe "when testing service availability" $ do
    it "should work when providing SAM host/port" $
      isAvailable "127.0.0.1" 7656 `shouldReturn` Available
    it "should return error when providing non-existing port" $
      isAvailable "127.0.0.1" 1234 `shouldReturn` ConnectionRefused
    it "should return error when providing invalid port" $
      isAvailable "127.0.0.1" 7657 `shouldReturn` IncorrectPort
