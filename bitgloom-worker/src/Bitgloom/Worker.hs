module Bitgloom.Worker where

import Database.Persist.Sql ( ConnectionPool
                            , runSqlPool)

import Control.Concurrent.Async ( mapConcurrently )
import Control.Concurrent.Async.Pool ( TaskGroup
                                     , createPool
                                     , createTaskGroup
                                     , async
                                     , runTaskGroup )

runWorker :: ConnectionPool -- ^ Access to our Sqlite database
          -> IO ()          -- ^ Should never return
runWorker dbPool = do
  taskPool     <- createPool

  -- The tasks that will be used for polling whether new jobs are
  -- inserted into the database.
  pollTasks <- createTaskGroup taskPool 1

  -- The tasks that handle the processing of the actual Jobs
  anonymizeTasks <- createTaskGroup taskPool 4

  -- Bootstrap initial tasks by polling the database  
  _ <- async pollTasks (pollTask dbPool pollTasks anonymizeTasks)  

  -- This should be a never-ending function, since runTaskGroup
  -- never returns, unless an exception is thrown; if this is the
  -- case, the other tasks are cancelled and the exception is
  -- rethrown.
  _ <- mapConcurrently runTaskGroup [pollTasks, anonymizeTasks]

  return ()

-- | Polls the database whether new jobs are submitted into the
--   database.
pollTask :: ConnectionPool -- ^ Access to our Sqlite database
         -> TaskGroup      -- ^ Access to the job queue for poll tasks
         -> TaskGroup      -- ^ Access to the job queue for anonymization tasks
         -> IO ()
pollTask dbPool pollTasks anonymizeTasks =
  putStrLn ("Now polling jobs")
