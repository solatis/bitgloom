module Handler.Status where

import Import
import qualified Bitgloom.I2P as I2P
import qualified Bitgloom.BTC as BTC
import qualified Data.Text as T (unpack)
import qualified Data.Text.Encoding as TE (encodeUtf8)

getStatusR :: Handler Html
getStatusR = undefined
--              do
--   master    <- getYesod
--   config    <- liftIO $ query (appConfiguration master) Config.QueryConfiguration
--   i2pStatus <- testI2P config
--   btcStatus <- testBTC config

--   let readyToServe = i2pStatus == I2P.Available && btcStatus == BTC.Available

--   defaultLayout $ do
--     setTitle "Status"
--     $(widgetFile "status")

-- -- | Validates whether I2P is available
-- testI2P :: MonadIO m => Config.ConfigurationState -> m I2P.Availability
-- testI2P config =
--   I2P.isAvailable ((Config.host . Config.i2pTcpEndpoint) config) ((Config.port . Config.i2pTcpEndpoint) config)

-- -- | Validates hwether BTC is available
-- testBTC :: MonadIO m => Config.ConfigurationState -> m BTC.Availability
-- testBTC config =
--   BTC.isAvailable
--     ((Config.host            . Config.btcEndpoint) config)
--     ((Config.port            . Config.btcEndpoint)  config)
--     ((TE.encodeUtf8          . Config.btcUsername) config)
--     ((TE.encodeUtf8          . Config.btcPassword) config)
