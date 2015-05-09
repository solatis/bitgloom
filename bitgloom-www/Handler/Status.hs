module Handler.Status where

import Import
import qualified Bitgloom.I2P as I2P
import qualified Bitgloom.BTC as BTC
import qualified Data.Text as T (unpack)

import Model.Configuration (retrieve)

getStatusR :: Handler Html
getStatusR = do
   master    <- getYesod
   config    <- runDB retrieve

   i2pStatus <- testI2P config
   btcStatus <- testBTC config

   let readyToServe = i2pStatus == I2P.Available && btcStatus == BTC.Available

   defaultLayout $ do
     setTitle "Status"
     $(widgetFile "status")

-- | Validates whether I2P is available
testI2P :: MonadIO m => Configuration -> m I2P.Availability
testI2P config =
  I2P.isAvailable ((T.unpack . configurationI2pTcpHost) config) (configurationI2pTcpPort config)

-- | Validates hwether BTC is available
testBTC :: MonadIO m => Configuration -> m BTC.Availability
testBTC config =
  BTC.isAvailable
    ((T.unpack . configurationBtcHost) config)
    (configurationBtcPort config)
    (configurationBtcUsername config)
    (configurationBtcPassword config)
