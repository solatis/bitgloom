module Handler.Configuration where

import Import hiding (update)

import qualified Data.Text as T (pack, unpack, concat)
import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)

import Data.Attoparsec.Text (char, decimal, skipSpace, endOfInput, parseOnly)

import Model.Configuration (retrieve, store)

getConfigurationR :: Handler Html
getConfigurationR = do
  master <- getYesod
  config <- runDB retrieve

  defaultLayout $ do
    setTitle "Configuration"
    $(widgetFile "configuration")

postConfigurationR :: Handler Html
postConfigurationR = do
  master <- getYesod

  config <- runInputPost $ Configuration
            <$> ireq textField "i2pTcpHost"
            <*> ireq intField "i2pTcpPort"
            <*> ireq textField "i2pUdpHost"
            <*> ireq intField "i2pUdpPort"
            <*> ireq textField "btcHost"
            <*> ireq intField "btcPort"
            <*> ireq textField "btcUsername"
            <*> ireq textField "btcPassword"

  runDB $ store config

  setMessage "Configuration has been stored"
  redirect ConfigurationR
