module Handler.Configuration where

import Import hiding (update)

import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack, unpack, concat, empty)
import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)

import Data.Attoparsec.Text (char, decimal, skipSpace, endOfInput, parseOnly)

import qualified Bitgloom.Driver.Model.Configuration as Model ( Configuration (..)
                                                              , retrieve
                                                              , store )

getConfigurationR :: Handler Html
getConfigurationR = do
  config <- runDB Model.retrieve

  (formWidget, formEnctype) <- generateFormPost (configurationForm config)

  defaultLayout $ do
    setTitle "Configuration"
    $(widgetFile "configuration")

postConfigurationR :: Handler Html
postConfigurationR = do
  config <- runDB Model.retrieve
  ((result, widget), enctype) <- runFormPost (configurationForm config)

  case result of
   FormSuccess config -> do
     runDB $ Model.store config
     setMessage "Configuration has been stored"
     redirect ConfigurationR

   FormMissing     -> error "Not a POST request!"
   FormFailure err -> error "Invalid form data!"

configurationForm :: Model.Configuration -> Html -> MForm Handler (FormResult Model.Configuration, Widget)
configurationForm config extra = do
  (torPortRes, torPortView)         <- mreq intField "Tor port" (Just (Model.configurationTorPort config))
  (btcHostRes, btcHostView)         <- mreq textField "Host" (Just (Model.configurationBtcHost config))
  (btcPortRes, btcPortView)         <- mreq intField "Port" (Just (Model.configurationBtcPort config))
  (btcUsernameRes, btcUsernameView) <- mreq textField "Username" (Just (Model.configurationBtcUsername config))
  (btcPasswordRes, btcPasswordView) <- mreq passwordField "Password" (Just (Model.configurationBtcPassword config))

  let configurationRes = Model.Configuration <$> torPortRes
                                             <*> btcHostRes
                                             <*> btcPortRes
                                             <*> btcUsernameRes
                                             <*> btcPasswordRes

  let widget = $(widgetFile "configuration-form")

  return (configurationRes, widget)
