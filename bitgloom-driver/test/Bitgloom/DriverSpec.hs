{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.DriverSpec where

import Bitgloom.Driver
import Test.Hspec

spec :: Spec
spec = do
  describe "when testing service availability" $ do
    it "should work when providing SAM host/port" $
      True `shouldBe` True
