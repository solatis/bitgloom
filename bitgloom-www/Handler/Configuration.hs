module Handler.Configuration where

import State.Configuration
import Import hiding (update)

import Data.Acid

getConfigurationR :: Handler Html
getConfigurationR = do
  master <- getYesod
  config <- liftIO $ query (appConfiguration master) QueryConfiguration

  defaultLayout $ do
    setTitle "Configuration"
    $(widgetFile "configuration")

postConfigurationR :: Handler Html
postConfigurationR = do
  master <- getYesod

  config <- runInputPost $ ConfigurationState
            <$> ireq textField "i2pTcpHost"
            <*> ireq intField  "i2pTcpPort"
            <*> ireq textField "i2pUdpHost"
            <*> ireq intField  "i2pUdpPort"
            <*> ireq textField "btcHost"
            <*> ireq intField  "btcPort"

  $(logDebug) "Now updating Configuration acid-state"

  liftIO $ update (appConfiguration master) (UpdateConfiguration config)

  setMessage "Configuration has been stored"
  redirect ConfigurationR
