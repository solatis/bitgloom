module Handler.Configuration where

import Import hiding (update)

import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack, unpack, concat, empty)
import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)

import Data.Attoparsec.Text (char, decimal, skipSpace, endOfInput, parseOnly)

import Model.Configuration (retrieve, store)

getConfigurationR :: Handler Html
getConfigurationR = do
  master <- getYesod
  config <- runDB retrieve

  (formWidget, formEnctype) <- generateFormPost (configurationForm config)

  defaultLayout $ do
    setTitle "Configuration"
    $(widgetFile "configuration")

postConfigurationR :: Handler Html
postConfigurationR = do
  config <- runDB retrieve
  ((result, widget), enctype) <- runFormPost (configurationForm config)

  case result of
   FormSuccess config -> do
     runDB $ store config
     setMessage "Configuration has been stored"
     redirect ConfigurationR

   FormMissing     -> error "Not a POST request!"
   FormFailure err -> error "Invalid form data!"

configurationForm :: Configuration -> Html -> MForm Handler (FormResult Configuration, Widget)
configurationForm config extra = do
  (i2pTcpHostRes, i2pTcpHostView)   <- mreq textField "TCP host" (Just (configurationI2pTcpHost config))
  (i2pTcpPortRes, i2pTcpPortView)   <- mreq intField "TCP port" (Just (configurationI2pTcpPort config))
  (i2pUdpHostRes, i2pUdpHostView)   <- mreq textField "UDP host" (Just (configurationI2pUdpHost config))
  (i2pUdpPortRes, i2pUdpPortView)   <- mreq intField "UDP port" (Just (configurationI2pUdpPort config))
  (btcHostRes, btcHostView)         <- mreq textField "Host" (Just (configurationBtcHost config))
  (btcPortRes, btcPortView)         <- mreq intField "Port" (Just (configurationBtcPort config))
  (btcUsernameRes, btcUsernameView) <- mreq textField "Username" (Just (configurationBtcUsername config))
  (btcPasswordRes, btcPasswordView) <- mreq passwordField "Password" (Just (configurationBtcPassword config))

  let configurationRes = Configuration <$> i2pTcpHostRes
                                       <*> i2pTcpPortRes
                                       <*> i2pUdpHostRes
                                       <*> i2pUdpPortRes
                                       <*> btcHostRes
                                       <*> btcPortRes
                                       <*> btcUsernameRes
                                       <*> btcPasswordRes

  let widget = $(widgetFile "configuration-form")

  return (configurationRes, widget)
