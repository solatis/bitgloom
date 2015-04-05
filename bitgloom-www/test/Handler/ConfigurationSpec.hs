module Handler.ConfigurationSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    it "submitting the form with working info is handled correctly" $ do
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
        -- more debugging printBody
        -- htmlCount ".message" 1
        -- htmlAllContain ".message" "Some Content"
        -- htmlAllContain ".message" "text/plain"
