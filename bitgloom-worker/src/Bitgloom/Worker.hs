module Bitgloom.Worker where

import Control.Monad ( void )

import Database.Persist.Sql ( ConnectionPool
                            , runSqlPool)

import Database.Persist.Class ( selectFirst
                              , update )
import Database.Persist ( (=.), (==.) )
import Database.Persist.Types ( Entity
                              , entityKey )

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( mapConcurrently )
import Control.Concurrent.Async.Pool ( TaskGroup
                                     , async
                                     , runTaskGroup )

import qualified Bitgloom.Driver.Model.Job as Job
import Bitgloom.Driver.Types ( JobState ( JobStateNew
                                        , JobStateAccepted ) )

newtype PollTaskGroup = PollTaskGroup { getPollTaskGroup :: TaskGroup }
newtype AnonymizeTaskGroup = AnonymizeTaskGroup { getAnonymizeTaskGroup :: TaskGroup }


runWorker :: PollTaskGroup      -- ^ The group for our Poll tasks
          -> AnonymizeTaskGroup -- ^ The group of our Anonymize tasks
          -> IO ()              -- ^ Should never return
runWorker pollTasks anonymizeTasks = 
  -- This should be a never-ending function, since runTaskGroup
  -- never returns, unless an exception is thrown; if this is the
  -- case, the other tasks are cancelled and the exception is
  -- rethrown.
  void $ mapConcurrently runTaskGroup [(getPollTaskGroup pollTasks), (getAnonymizeTaskGroup anonymizeTasks)]

-- | Submits a new poll task which, in addition to processing the Job that
--   is polled, submits a new poll Job after 3 seconds.
submitRecursivePollTask :: ConnectionPool     -- ^ Access to our Sqlite database
                        -> PollTaskGroup      -- ^ The group of our Poll tasks
                        -> (Entity Job.Job -> IO ()) -- ^ Function to be called for each incoming job
                        -> IO ()
submitRecursivePollTask dbPool pollTasks f = do
  void $ async (getPollTaskGroup pollTasks) (pollTask dbPool (submitNext))

  where
    submitNext job = do
      -- Execute callback, no matter what
      f job

      -- Wait 3 seconds, and then recursively submit new job
      threadDelay 3000000
      submitRecursivePollTask dbPool pollTasks f

-- | Polls the database whether new jobs are submitted into the
--   database.
pollTask :: ConnectionPool     -- ^ Access to our Sqlite database
         -> (Entity Job.Job -> IO ()) -- ^ Function to be called for each incoming job
         -> IO ()
pollTask dbPool f = do
  putStrLn ("Now polling jobs")

  maybeJob <- runSqlPool acceptJob dbPool

  case maybeJob of
   (Just job) -> f job
   Nothing    -> return ()

  where
    acceptJob = do
      res <- selectFirst ([Job.JobState ==. JobStateNew]) []
      case res of
       Just job -> do
         update (entityKey job) [Job.JobState =. JobStateAccepted]
         return (Just job)
         
       Nothing  -> return Nothing
