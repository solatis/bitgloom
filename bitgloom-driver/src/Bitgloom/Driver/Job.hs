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

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import qualified Control.Concurrent.Async.Pool as Pool 
import qualified Bitgloom.Driver.Model.Job      as Job

worker :: ConnectionPool
       -> Key Job.Job
       -> IO ()
worker pool job =
  putStrLn ("Now launching job: " <> tshow job)
