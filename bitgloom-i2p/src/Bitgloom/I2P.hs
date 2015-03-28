module Bitgloom.I2P where

import Control.Monad.IO.Class
import Control.Monad.Catch (handle, handleIOError)
import Control.Concurrent.MVar
import qualified System.IO.Error as E
import qualified GHC.IO.Exception as E

import Network.Anonymous.I2P (defaultEndPoint, withSession)
import Network.Anonymous.I2P.Types.Sam
import Network.Anonymous.I2P.Types.Socket
import Network.Anonymous.I2P.Error

data Availability =
  Available |
  ConnectionRefused |
  IncorrectPort
  deriving (Show, Eq)

isAvailable :: MonadIO m => String -> Int -> m Availability
isAvailable host port =
  let
    -- We can only test the TCP host/port of the same bridge, since there
    -- is no way to know whether an UDP message arrived, unless we are using
    -- very sophisticated tests.
    --
    -- As such, the best solution is to just use the default udp host/port
    endPoint :: EndPoints
    endPoint = defaultEndPoint { tcp = (host, show port) }

  in do
    errMsg <- liftIO $ newEmptyMVar

    liftIO $
      handle (\(I2PError ProtocolError) -> putMVar errMsg IncorrectPort)
      $ handleIOError (\e  ->
                        -- The error raised for a Connection Refused is a very descriptive OtherError
                        if   (E.ioeGetErrorType e) == E.OtherError
                        then (putMVar errMsg ConnectionRefused)
                        else E.ioError e)
        (withSession endPoint VirtualStream (\_ ->
                                              liftIO $ putMVar errMsg Available))

    res <- liftIO $ takeMVar errMsg
    return res