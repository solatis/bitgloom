module Handler.Status where

import Import
import State.Configuration

import Data.Acid

getStatusR :: Handler Html
getStatusR = do
  master <- getYesod
  config <- liftIO $ query (appConfiguration master) QueryConfiguration
  errorE <- testServices config

  defaultLayout $ do
    setTitle "Status"
    $(widgetFile "status")

-- | Validates whether all services are reachable
testServices :: MonadIO m => ConfigurationState -> m (Either String ())
testServices config =
  let testI2p :: String -> Int -> m (Either String ())
      testI2p host port = undefined

  in do
    i2pWorks <- testI2p (i2pHost config) (i2pPort config)

    return (i2pWorks)
