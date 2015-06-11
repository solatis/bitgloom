module Handler.AnonymizeAccount ( getAnonymizeAccountR
                                , postAnonymizeAccountR ) where

import Import

import qualified Bitgloom.BTC as Btc
import qualified Bitgloom.Driver.Types as Driver
import qualified Data.Bitcoin.Types as BT
import Data.Bifunctor (first)

import qualified Data.Text as T (Text, pack, unpack)
import Text.Read (readEither)

import Debug.Trace (trace)
import Model.Configuration (retrieve)

getAnonymizeAccountR :: T.Text -> Handler Html
getAnonymizeAccountR accountId = do
  config  <- runDB retrieve

  (formWidget, formEnctype) <- generateFormPost (anonymizeForm config accountId)

  defaultLayout $ do
    setTitle "Anonymize Account"
    $(widgetFile "anonymize-account")

postAnonymizeAccountR :: T.Text -> Handler Html
postAnonymizeAccountR accountId = do
  $logDebug "I'm in postAnonymizeAccountR"

  config <- runDB retrieve
  ((result, widget), enctype) <- runFormPost (anonymizeForm config accountId)

  $logDebug ("I'm in postAnonymizeAccountR, result = " <> (T.pack $ show result))

  setMessage "Anonymization has started"
  redirect (AnonymizeAccountR accountId)

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

  (iterationsRes, iterationsView) <- mreq intField "Minimum amount of rounds" (Just 10)
  (amountRes, amountView)         <- mreq btcField "BTC to anonymize" (Just balance)
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

btcField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m BT.Btc
btcField = Field
    { fieldParse   = parseHelper parse
    , fieldView    = view
    , fieldEnctype = UrlEncoded
    }

  where
    view theId name attrs val isReq = toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="number" step=any :isReq:required="" value="#{showVal val}">
|]
    showVal   = either id (pack . show)
    parse str = first (MsgInvalidEntry . T.pack) $ readEither (T.unpack str)
