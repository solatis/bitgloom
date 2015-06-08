module Bitgloom.Driver.Types where

import qualified Data.Bitcoin.Types as BT
import Data.Fixed (Centi)
import Data.Word (Word8)

data Role = Advertiser | Respondent deriving (Bounded, Enum, Show, Eq)

data BlockSize = BlockSize01  -- ^ 0.1 BTC
               | BlockSize1   -- ^   1 BTC
               | BlockSize10  -- ^  10 BTC
               | BlockSize100 -- ^ 100 BTC

               deriving (Eq, Enum, Bounded)

instance Show BlockSize where
  show BlockSize01  = "0.1 BTC"
  show BlockSize1   = "1 BTC"
  show BlockSize10  = "10 BTC"
  show BlockSize100 = "100 BTC"

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
  amount :: BlockSize,
  fee :: Fee
  }
