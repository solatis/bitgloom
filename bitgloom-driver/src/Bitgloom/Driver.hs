module Bitgloom.Driver where

import Crypto.Random (CPRG)
import Crypto.Number.Generate (generateBetween)

import Bitgloom.Driver.Types (Role (..))

pickRole :: CPRG a => a -> (Role, a)
pickRole g =
  case generateBetween g (toInteger (fromEnum (minBound :: Role))) (toInteger (fromEnum (maxBound :: Role))) of
   (i, g') -> (toEnum (fromIntegral i), g')
