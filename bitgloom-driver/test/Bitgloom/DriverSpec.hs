{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.DriverSpec where

import Crypto.Random (  CPRG
                      , SystemRNG
                      , createEntropyPool
                      , cprgCreate)

import Data.List (nub)
import Bitgloom.Driver
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
