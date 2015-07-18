{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.WorkerSpec where

import Control.Monad.Logger ( runStdoutLoggingT )

import Database.Persist.Sql ( ConnectionPool
                            , runMigration
                            , runSqlPool )
import Database.Persist.Sqlite ( createSqlitePool )


import Bitgloom.Driver.Types (Fee (Fee1))
import qualified Bitgloom.Driver.Model as Model ( migrate )
import qualified Bitgloom.Driver.Model.Job as Job ( Job (..)
                                                  , store )
import qualified Bitgloom.Worker as Worker ( runWorker )

import Test.Hspec
import Test.Hspec.Expectations


-- | Wraps a call around an in-memory sqlite pool, and ensures that the
--   necessary tables are created.
withDb :: (ConnectionPool -> IO a)
       -> IO a
withDb cb = do
  pool <- runStdoutLoggingT $ do
    pool <- createSqlitePool ":memory:" 1
    runSqlPool (runMigration Model.migrate) pool
    return pool

  cb pool

spec :: Spec
spec = 
  describe "when creating a new job" $ do
    it "worker should pick up the job within 3 seconds" $
      let job = Job.Job 10 10 Fee1

      in withDb $ \pool -> do
        Worker.runWorker pool
        _ <- runSqlPool (Job.store job) pool
        return ()
                

      
