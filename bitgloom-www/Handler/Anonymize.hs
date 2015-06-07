module Handler.Anonymize ( getAnonymizeR ) where

import Import
import qualified Bitgloom.BTC as Btc
import qualified Data.Text as T (unpack)

import Model.Configuration (retrieve)

getAnonymizeR :: Handler Html
getAnonymizeR = do
  config <- runDB retrieve

  accounts <- liftIO $ connectBtc config Btc.listAccounts

  defaultLayout $ do
    setTitle "Anonymize"
    $(widgetFile "anonymize")

-- | Validates hwether BTC is available
connectBtc :: Configuration -> (Btc.Client -> IO a) -> IO a
connectBtc config =
  Btc.withClient
    ((T.unpack . configurationBtcHost) config)
    (configurationBtcPort config)
    (configurationBtcUsername config)
    (configurationBtcPassword config)
