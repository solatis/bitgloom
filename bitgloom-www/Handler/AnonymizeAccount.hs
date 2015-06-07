module Handler.AnonymizeAccount ( getAnonymizeAccountR ) where

import Import
import qualified Bitgloom.BTC as Btc
import qualified Data.Text as T (Text, unpack)

import Model.Configuration (retrieve)

getAnonymizeAccountR :: T.Text -> Handler Html
getAnonymizeAccountR account = do
  config <- runDB retrieve

  accounts <- liftIO $ connectBtc config Btc.listAccounts

  defaultLayout $ do
    setTitle "Anonymize Account"
    $(widgetFile "anonymize-account")

connectBtc :: Configuration -> (Btc.Client -> IO a) -> IO a
connectBtc config =
  Btc.withClient
    ((T.unpack . configurationBtcHost) config)
    (configurationBtcPort config)
    (configurationBtcUsername config)
    (configurationBtcPassword config)
