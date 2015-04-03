{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module State.Configuration where

import ClassyPrelude

import Control.Monad.State

import Data.Acid
import Data.SafeCopy
import Data.Text

type Host = String
type Port = Int

data Endpoint = Endpoint {
  host :: Host,
  port :: Port
  } deriving (Typeable)

instance Show Endpoint where
  show (Endpoint host port) = host ++ ":" ++ show port

data ConfigurationState = ConfigurationState {
  i2pTcpEndpoint :: Endpoint,
  i2pUdpEndpoint :: Endpoint,

  btcEndpoint    :: Endpoint,
  btcUsername    :: Text,
  btcPassword    :: Text
  } deriving (Show, Typeable)

defaultConfiguration :: ConfigurationState
defaultConfiguration =
  ConfigurationState (Endpoint "127.0.0.1" 7656) (Endpoint "127.0.0.1" 7655) (Endpoint "127.0.0.1" 8332) empty empty

$(deriveSafeCopy 0 'base ''Endpoint)
$(deriveSafeCopy 0 'base ''ConfigurationState)

updateConfiguration :: ConfigurationState -> Update ConfigurationState ()
updateConfiguration = put

queryConfiguration :: Query ConfigurationState ConfigurationState
queryConfiguration = ask

$(makeAcidic ''ConfigurationState ['updateConfiguration, 'queryConfiguration])
