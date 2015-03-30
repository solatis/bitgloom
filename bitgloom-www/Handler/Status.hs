module Handler.Status where

import Import
import State.Configuration
import qualified Bitgloom.I2P as I2P
import qualified Data.Text as T (unpack)

import Data.Acid

getStatusR :: Handler Html
getStatusR = do
  master    <- getYesod
  config    <- liftIO $ query (appConfiguration master) QueryConfiguration
  i2pStatus <- testI2P config

  let readyToServe = (i2pStatus == I2P.Available)

  defaultLayout $ do
    setTitle "Status"
    $(widgetFile "status")

-- | Validates whether I2P is available
testI2P :: MonadIO m => ConfigurationState -> m I2P.Availability
testI2P config =
  I2P.isAvailable ((T.unpack . i2pTcpHost) config) (i2pTcpPort config)
