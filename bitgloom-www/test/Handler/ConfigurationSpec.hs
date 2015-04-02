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
            fileByLabel "Choose a file" "test/Spec.hs" "text/plain" -- talk about self-reference
            byLabel "What's on the file?" "Some Content"

        statusIs 200
        -- more debugging printBody
        htmlCount ".message" 1
        htmlAllContain ".message" "Some Content"
        htmlAllContain ".message" "text/plain"
