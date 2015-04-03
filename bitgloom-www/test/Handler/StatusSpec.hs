module Handler.StatusSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    it "loads the index and checks it looks right" $ do
        get StatusR
        statusIs 200
