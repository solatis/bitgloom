module Bitgloom.Driver.Types where

import qualified Data.Bitcoin.Types as BT
import Data.Fixed (Centi)
import Data.Word (Word8)

data Role = Advertiser | Respondent deriving (Show, Eq, Bounded, Enum)

data Fee = Fee001 -- ^ 0.01 %
         | Fee01  -- ^ 0.1 %
         | Fee1   -- ^ 1 %

         deriving (Eq, Enum, Bounded)

instance Show Fee where
  show Fee001 = "0.01 %"
  show Fee01  = "0.1 %"
  show Fee1   = "1 %"

data Job = Job {
  iterations :: Word8,
  amount :: BT.Btc,
  fee :: Fee
  } deriving (Show, Eq)
