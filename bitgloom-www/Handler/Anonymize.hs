module Handler.Anonymize ( getAnonymizeR ) where

import Import
import qualified Bitgloom.BTC as Btc
import qualified Data.Text as T (unpack)

import qualified Bitgloom.Driver.Model.Configuration as Model

getAnonymizeR :: Handler Html
getAnonymizeR = do
  config <- runDB Model.retrieve

  accounts <- liftIO $ connectBtc config Btc.listAccounts

  defaultLayout $ do
    setTitle "Anonymize"
    $(widgetFile "anonymize")

-- | Validates hwether BTC is available
connectBtc :: Model.Configuration -> (Btc.Client -> IO a) -> IO a
connectBtc config =
  Btc.withClient
    ((T.unpack . Model.configurationBtcHost) config)
    (Model.configurationBtcPort config)
    (Model.configurationBtcUsername config)
    (Model.configurationBtcPassword config)
