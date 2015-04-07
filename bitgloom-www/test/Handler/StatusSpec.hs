module Handler.StatusSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "checks for ready to serve button with proper configuration" $ do
        get ConfigurationR
        statusIs 200

        request $ do
          setMethod "POST"
          setUrl ConfigurationR
          addToken
          byLabel "TCP host" "127.0.0.1"
          byLabel "TCP port" "7656"
          byLabel "UDP host" "127.0.0.1"
          byLabel "UDP port" "7655"
          byLabel "Host" "127.0.0.1"
          byLabel "Port" "18332"
          byLabel "Username" "user"
          byLabel "Password" "pass"

        statusIs 303

        get StatusR
        statusIs 200

        htmlCount ".special" 1
        htmlAllContain ".special" "Ready to serve"

    it "checks for absence of ready to serve when providing incorrect i2p config" $ do
        get ConfigurationR
        statusIs 200

        request $ do
          setMethod "POST"
          setUrl ConfigurationR
          addToken
          byLabel "TCP host" "127.0.0.1"
          byLabel "TCP port" "7657"
          byLabel "UDP host" "127.0.0.1"
          byLabel "UDP port" "7655"
          byLabel "Host" "127.0.0.1"
          byLabel "Port" "18332"
          byLabel "Username" "user"
          byLabel "Password" "pass"

        statusIs 303

        get StatusR
        statusIs 200

        htmlCount ".special" 0

    it "checks for absence of ready to serve when providing incorrect btc config" $ do
        get ConfigurationR
        statusIs 200

        request $ do
          setMethod "POST"
          setUrl ConfigurationR
          addToken
          byLabel "TCP host" "127.0.0.1"
          byLabel "TCP port" "7656"
          byLabel "UDP host" "127.0.0.1"
          byLabel "UDP port" "7655"
          byLabel "Host" "127.0.0.1"
          byLabel "Port" "8332"
          byLabel "Username" "user"
          byLabel "Password" "pass"

        statusIs 303

        get StatusR
        statusIs 200

        htmlCount ".special" 0
