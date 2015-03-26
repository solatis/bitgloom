{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module State.Configuration where

import ClassyPrelude

import Control.Monad.State

import Data.Acid
import Data.SafeCopy
import Data.Text

type Host = Text
type Port = Int

data ConfigurationState = ConfigurationState {
  i2pHost :: Host,
  i2pPort :: Port,

  btcHost :: Host,
  btcPort :: Port
  } deriving (Show, Typeable)

defaultConfiguration :: ConfigurationState
defaultConfiguration =
  ConfigurationState "127.0.0.1" 7656 "127.0.0.1" 8333

$(deriveSafeCopy 0 'base ''ConfigurationState)

updateConfiguration :: ConfigurationState -> Update ConfigurationState ()
updateConfiguration = put

queryConfiguration :: Query ConfigurationState ConfigurationState
queryConfiguration = ask

$(makeAcidic ''ConfigurationState ['updateConfiguration, 'queryConfiguration])
