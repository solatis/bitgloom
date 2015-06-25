{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitgloom.Driver.Job where

import ClassyPrelude.Yesod

import qualified Bitgloom.Driver.Model.Job           as Job

create :: (MonadIO m, Functor m)
       => Job.Job
       -> ReaderT SqlBackend m (Key Job.Job)
create job = do
  jobId <- insert job

  -- | _ <- liftIO $ forkSupervised supervisor oneForOne (worker job)

  return jobId

worker :: Job.Job
       -> IO ()
worker job =
  putStrLn ("Now launching job: " <> tshow job)
