{-# LANGUAGE TemplateHaskell            #-}

module Bitgloom.Driver.Types where

import qualified Data.Bitcoin.Types as BT
import Data.Word (Word8)

import Database.Persist.TH

data Role = Advertiser
          | Respondent

          deriving (Show, Read, Eq, Bounded, Enum)

derivePersistField "Role"

data Fee = Fee001 -- ^ 0.01 %
         | Fee01  -- ^ 0.1 %
         | Fee1   -- ^ 1 %

         deriving (Eq, Enum, Bounded)

instance Show Fee where
  show Fee001 = "0.01 %"
  show Fee01  = "0.1 %"
  show Fee1   = "1 %"

instance Read Fee where
  readsPrec _ "0.01 %" = [(Fee001, "")]
  readsPrec _ "0.1 %" = [(Fee01, "")]
  readsPrec _ "1 %" = [(Fee1, "")]

derivePersistField "Fee"

data JobState =
  JobStateNew |
  JobStateAccepted |
  JobStateWorking |
  JobStateFailure |
  JobStateSuccess
  deriving (Eq, Show, Read)

derivePersistField "JobState"
