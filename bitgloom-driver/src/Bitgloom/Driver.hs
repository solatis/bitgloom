module Bitgloom.Driver where

import Crypto.Random (CPRG)
import Crypto.Number.Generate (generateBetween)

data Role = Advertiser | Respondent deriving (Bounded, Enum, Show, Eq)

pickRole :: CPRG a => a -> (Role, a)
pickRole g =
  case generateBetween g (toInteger (fromEnum (minBound :: Role))) (toInteger (fromEnum (maxBound :: Role))) of
   (i, g') -> (toEnum (fromIntegral i), g')
