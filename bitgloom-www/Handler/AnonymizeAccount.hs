module Handler.AnonymizeAccount ( getAnonymizeAccountR
                                , postAnonymizeAccountR ) where

import Import

import qualified Bitgloom.BTC as Btc
import qualified Bitgloom.Driver.Types as Driver
import qualified Data.Bitcoin.Types as BT
import Data.Bifunctor (first)

import qualified Data.Text as T (Text, pack, unpack)
import Text.Read (readEither)

import qualified Bitgloom.Driver.Model.Configuration as Model ( Configuration (..)
                                                              , retrieve)
import qualified Bitgloom.Driver.Model.Job as Model ( Job (..) )

import Debug.Trace (trace)

getAnonymizeAccountR :: T.Text -> Handler Html
getAnonymizeAccountR accountId = do
  config  <- runDB Model.retrieve

  (formWidget, formEnctype) <- generateFormPost (anonymizeForm config accountId)

  defaultLayout $ do
    setTitle "Anonymize Account"
    $(widgetFile "anonymize-account")

postAnonymizeAccountR :: T.Text -> Handler Html
postAnonymizeAccountR accountId = do
  config <- runDB Model.retrieve
  ((result, widget), enctype) <- runFormPost (anonymizeForm config accountId)

  case result of
   FormSuccess job -> do
     runDB $ insert job
     setMessage "Configuration has been stored"
     redirect ConfigurationR

   FormMissing     -> error "Not a POST request!"
   FormFailure err -> error "Invalid form data!"

connectBtc :: Model.Configuration -> (Btc.Client -> IO a) -> IO a
connectBtc config =
  Btc.withClient
    ((T.unpack . Model.configurationBtcHost) config)
    (Model.configurationBtcPort config)
    (Model.configurationBtcUsername config)
    (Model.configurationBtcPassword config)

anonymizeForm :: Model.Configuration -> T.Text -> Html -> MForm Handler (FormResult Model.Job, Widget)
anonymizeForm config accountId extra = do
  balance <- liftIO $ getBalance config accountId

  (iterationsRes, iterationsView) <- mreq intField "Minimum amount of rounds" (Just 10)
  (amountRes, amountView)         <- mreq btcField "BTC to anonymize" (Just balance)
  (percentageRes, percentageView) <- mreq (selectFieldList feeTypes) "Fee %" (Just Driver.Fee01)

  let anonymizeJob = Model.Job <$> iterationsRes
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
