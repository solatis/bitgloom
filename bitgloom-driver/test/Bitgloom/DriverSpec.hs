{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.DriverSpec where

import Crypto.Random (  CPRG
                      , SystemRNG
                      , createEntropyPool
                      , cprgCreate)

import Data.List (nub)

import Bitgloom.Driver
import Bitgloom.Driver.Types

import Test.Hspec
import Test.Hspec.Expectations

spec :: Spec
spec = do
  describe "when picking a role" $ do
    it "should be pick up both roles after 10 attempts" $
      let cprg pool       = cprgCreate pool :: SystemRNG

          -- Generates infinite list of roles
          randomRoles gen =
            role : (randomRoles g')

            where
              (role, g') = pickRole gen

      in do
         entPool <- createEntropyPool
         let res = nub $ take 10 (randomRoles (cprg entPool))

         elem Advertiser res `shouldBe` True
         elem Respondent res `shouldBe` True

  describe "when serializing a fee" $ do
    it "should be able to be read back" $ do
      (read . show) Fee001 `shouldBe` Fee001
      (read . show) Fee01 `shouldBe` Fee01
      (read . show) Fee1 `shouldBe` Fee1
