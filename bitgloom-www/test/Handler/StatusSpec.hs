module Handler.StatusSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    it "loads the index and checks for ready to serve button" $ do
        get StatusR
        statusIs 200

        htmlCount ".special" 1
        htmlAllContain ".special" "Ready to serve"
