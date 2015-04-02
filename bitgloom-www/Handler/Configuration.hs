module Handler.Configuration where

import State.Configuration
import Import hiding (update)

import qualified Data.Text as T (pack, unpack, concat)

import Data.Attoparsec.Text (char, decimal, skipSpace, endOfInput, parseOnly)
import Data.Acid

getConfigurationR :: Handler Html
getConfigurationR = do
  master <- getYesod
  config <- liftIO $ query (appConfiguration master) QueryConfiguration

  defaultLayout $ do
    setTitle "Configuration"
    $(widgetFile "configuration")

postConfigurationR :: Handler Html
postConfigurationR = do
  master <- getYesod

  config <- runInputPost $ ConfigurationState
            <$> ireq endpointField "i2pTcpEndpoint"
            <*> ireq endpointField "i2pUdpEndpoint"
            <*> ireq endpointField "btcEndpoint"
            <*> ireq textField "btcUsername"
            <*> ireq textField "btcPassword"

  $(logDebug) "Now updating Configuration acid-state"

  liftIO $ update (appConfiguration master) (UpdateConfiguration config)

  setMessage "Configuration has been stored"
  redirect ConfigurationR

endpointField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Endpoint
endpointField =
  Field
    { fieldParse = parseHelper $ parseEndpoint
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet| <input id="#{theId}" name="#{name}" *{attrs} type="number" step=any :isReq:required="" value="#{showVal val}"> |]
    , fieldEnctype = UrlEncoded
    }
  where showVal           = either id (pack . show)
        parseEndpoint str =
          case (parseOnly endpointParser str) of
            Left _         -> Left (MsgInvalidEntry (T.concat [T.pack "Endpoint is not in an ip:port format: ", str]))
            Right endpoint -> Right endpoint

        endpointParser = do
          skipSpace

          ip1  <- decimal
          _    <- char '.'
          ip2  <- decimal
          _    <- char '.'
          ip3  <- decimal
          _    <- char '.'
          ip4  <- decimal
          _    <- char ':'

          port <- decimal
          skipSpace
          endOfInput

          return $ Endpoint ((show ip1) ++ "." ++ (show ip2) ++ "." ++ (show ip3) ++ "." ++ (show ip4)) port
