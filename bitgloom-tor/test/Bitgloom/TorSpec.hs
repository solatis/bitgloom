{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.TorSpec where

import Bitgloom.Tor
import Test.Hspec

spec :: Spec
spec = do
  describe "when testing service availability" $ do
    it "should work when providing valid Tor control ports" $ do
      ports <- detectPort [9051, 9151]
      length (ports) `shouldBe` 1

    it "should return error when providing invalid port" $ do
      ports <- detectPort [9050]
      length (ports) `shouldBe` 0

    it "should return error when providing unavailable port" $ do
      ports <- detectPort [1234]
      length (ports) `shouldBe` 0
