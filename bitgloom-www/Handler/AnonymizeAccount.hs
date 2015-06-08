module Handler.AnonymizeAccount ( getAnonymizeAccountR
                                , postAnonymizeAccountR ) where

import Import
import qualified Bitgloom.BTC as Btc
import qualified Bitgloom.Driver.Types as Driver

import qualified Data.Text as T (Text, unpack)

import Model.Configuration (retrieve)

getAnonymizeAccountR :: T.Text -> Handler Html
getAnonymizeAccountR accountId = do
  config  <- runDB retrieve

  (formWidget, formEnctype) <- generateFormPost (anonymizeForm config accountId)

  defaultLayout $ do
    setTitle "Anonymize Account"
    $(widgetFile "anonymize-account")

postAnonymizeAccountR :: T.Text -> Handler Html
postAnonymizeAccountR accountId = undefined

connectBtc :: Configuration -> (Btc.Client -> IO a) -> IO a
connectBtc config =
  Btc.withClient
    ((T.unpack . configurationBtcHost) config)
    (configurationBtcPort config)
    (configurationBtcUsername config)
    (configurationBtcPassword config)

anonymizeForm :: Configuration -> T.Text -> Html -> MForm Handler (FormResult Driver.Job, Widget)
anonymizeForm config accountId extra = do
  balance <- liftIO $ getBalance config accountId

  (iterationsRes, iterationsView) <- mreq intField "Amount of rounds" (Just 10)
  (amountRes, amountView)         <- mreq (selectFieldList blockSizeTypes) "BTC per round" (Just Driver.BlockSize1)
  (percentageRes, percentageView) <- mreq (selectFieldList feeTypes) "Fee %" (Just Driver.Fee01)

  let anonymizeJob = Driver.Job <$> iterationsRes
                                <*> amountRes
                                <*> percentageRes

  let widget = $(widgetFile "anonymize-account-form")

  return (anonymizeJob, widget)

  where
    getBalance config accountId =
      connectBtc config (`Btc.getAccountBalance` accountId)

    feeTypes :: [(Text, Driver.Fee)]
    feeTypes = map (pack . show &&& id) [minBound..maxBound]

    blockSizeTypes :: [(Text, Driver.BlockSize)]
    blockSizeTypes = map (pack . show &&& id) [minBound..maxBound]
