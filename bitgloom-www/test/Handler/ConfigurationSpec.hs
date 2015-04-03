module Handler.ConfigurationSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    it "loads the index and checks it looks right" $ do
        get ConfigurationR
        statusIs 200

        request $ do
          setMethod "POST"
          setUrl ConfigurationR
          addToken
          byLabel "TCP endpoint" "127.0.0.1:7656"
          byLabel "UDP endpoint" "127.0.0.1:7655"
          byLabel "Endpoint" "127.0.0.1:18332"
          byLabel "Username" "user"
          byLabel "Password" "pass"

        statusIs 200
        -- more debugging printBody
        -- htmlCount ".message" 1
        -- htmlAllContain ".message" "Some Content"
        -- htmlAllContain ".message" "text/plain"
