module Handler.Anonymize where

import Import
import qualified Bitgloom.BTC as BTC

import Model.Configuration (retrieve)

getAnonymizeR :: Handler Html
getAnonymizeR =
   defaultLayout $ do
     setTitle "Anonymize"
     $(widgetFile "Anonymize")
