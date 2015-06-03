module Handler.Status where

import Import
import qualified Bitgloom.Tor as Tor
import qualified Bitgloom.BTC as BTC
import qualified Data.Text as T (unpack)

import Model.Configuration (retrieve)

getStatusR :: Handler Html
getStatusR = do
   master    <- getYesod
   config    <- runDB retrieve

   torStatus <- testTor config
   btcStatus <- testBTC config

   let readyToServe = torStatus == Tor.Available && btcStatus == BTC.Available

   defaultLayout $ do
     setTitle "Status"
     $(widgetFile "status")

-- | Validates whether Tor is available
testTor :: MonadIO m => Configuration -> m Tor.Availability
testTor config =
  Tor.isAvailable ((toInteger . configurationTorPort) config)

-- | Validates hwether BTC is available
testBTC :: MonadIO m => Configuration -> m BTC.Availability
testBTC config =
  BTC.isAvailable
    ((T.unpack . configurationBtcHost) config)
    (configurationBtcPort config)
    (configurationBtcUsername config)
    (configurationBtcPassword config)
