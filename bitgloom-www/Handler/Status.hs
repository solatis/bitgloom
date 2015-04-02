module Handler.Status where

import Import
import State.Configuration
import qualified Bitgloom.I2P as I2P
import qualified Bitgloom.BTC as BTC
import qualified Data.Text as T (unpack)
import qualified Data.Text.Encoding as TE (encodeUtf8)

import Data.Acid

getStatusR :: Handler Html
getStatusR = do
  master    <- getYesod
  config    <- liftIO $ query (appConfiguration master) QueryConfiguration
  i2pStatus <- testI2P config
  btcStatus <- testBTC config

  let readyToServe = i2pStatus == I2P.Available && btcStatus == BTC.Available

  defaultLayout $ do
    setTitle "Status"
    $(widgetFile "status")

-- | Validates whether I2P is available
testI2P :: MonadIO m => ConfigurationState -> m I2P.Availability
testI2P config =
  I2P.isAvailable ((T.unpack . i2pTcpHost) config) (i2pTcpPort config)

-- | Validates hwether BTC is available
testBTC :: MonadIO m => ConfigurationState -> m BTC.Availability
testBTC config =
  BTC.isAvailable ((T.unpack . btcHost) config) (btcPort config) ((TE.encodeUtf8 . btcUsername) config) ((TE.encodeUtf8 . btcPassword) config)
