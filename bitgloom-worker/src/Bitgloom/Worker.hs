module Bitgloom.Worker where

import Control.Monad ( void )

import Database.Persist.Sql ( ConnectionPool
                            , runSqlPool)

import Control.Concurrent.Async ( mapConcurrently )
import Control.Concurrent.Async.Pool ( TaskGroup
                                     , async
                                     , runTaskGroup )

newtype PollTaskGroup = PollTaskGroup { getPollTaskGroup :: TaskGroup }
newtype AnonymizeTaskGroup = AnonymizeTaskGroup { getAnonymizeTaskGroup :: TaskGroup }


runWorker :: ConnectionPool     -- ^ Access to our Sqlite database
          -> PollTaskGroup      -- ^ The group for our Poll tasks
          -> AnonymizeTaskGroup -- ^ The group of our Anonymize tasks
          -> IO ()              -- ^ Should never return
runWorker dbPool pollTasks anonymizeTasks = do
  -- Bootstrap initial tasks by polling the database 
  submitPollTask dbPool pollTasks anonymizeTasks

  -- This should be a never-ending function, since runTaskGroup
  -- never returns, unless an exception is thrown; if this is the
  -- case, the other tasks are cancelled and the exception is
  -- rethrown.
  _ <- mapConcurrently runTaskGroup [(getPollTaskGroup pollTasks), (getAnonymizeTaskGroup anonymizeTasks)]

  return ()

-- | Generates a new Poll task and submits it onto the correct
--   TaskGroup
submitPollTask :: ConnectionPool     -- ^ Access to our Sqlite database
               -> PollTaskGroup      -- ^ The group of our Poll tasks
               -> AnonymizeTaskGroup -- ^ The group of our Anonymize tasks
               -> IO ()
submitPollTask dbPool pollTasks anonymizeTasks =
  void $ async (getPollTaskGroup pollTasks) (pollTask dbPool pollTasks anonymizeTasks)

-- | Polls the database whether new jobs are submitted into the
--   database.
pollTask :: ConnectionPool     -- ^ Access to our Sqlite database
         -> PollTaskGroup      -- ^ Access to the job queue for poll tasks
         -> AnonymizeTaskGroup -- ^ Access to the job queue for anonymization tasks
         -> IO ()
pollTask dbPool pollTasks anonymizeTasks =
  putStrLn ("Now polling jobs")
