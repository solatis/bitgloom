{-# LANGUAGE OverloadedStrings #-}

module Bitgloom.WorkerSpec where

import Control.Monad.Logger ( runStdoutLoggingT )

import Database.Persist.Sql ( ConnectionPool
                            , runMigration
                            , runSqlPool )
import Database.Persist.Class ( Key )
  
import Database.Persist.Sqlite ( createSqlitePool )


import Control.Concurrent ( forkIO
                          , killThread )
import Control.Concurrent.Async.Pool ( TaskGroup
                                     , createPool
                                     , createTaskGroup
                                     , async
                                     , wait
                                     , runTaskGroup )

import Bitgloom.Driver.Types (Fee (Fee1), JobState (JobStateAccepted) )
import qualified Bitgloom.Driver.Model as Model ( migrate )
import qualified Bitgloom.Driver.Model.Job as Job
  
import qualified Bitgloom.Worker as Worker ( AnonymizeTaskGroup (..)
                                           , PollTaskGroup (..)
                                           , runWorker
                                           , pollTask )

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
      let job = Job.Job 10 10 Fee1 Job.defaultJobState

          jobState :: ConnectionPool -> (Key Job.Job) -> IO JobState
          jobState pool key = do
            job <- runSqlPool (Job.retrieve  key) pool
            return $ Job.jobState job

      in withDb $ \pool -> do
        taskPool <- createPool
        pollTaskGroup <- createTaskGroup taskPool 1
        anonymizeTaskGroup <- createTaskGroup taskPool 4
        
        tid <- forkIO $ Worker.runWorker (Worker.PollTaskGroup pollTaskGroup) (Worker.AnonymizeTaskGroup anonymizeTaskGroup)
        fk <- runSqlPool (Job.store job) pool

        future <- async pollTaskGroup (Worker.pollTask pool (\_ -> putStrLn ("received job!")))
        wait future

        killThread tid

        (jobState pool fk) `shouldReturn` JobStateAccepted

        return ()
                

      
