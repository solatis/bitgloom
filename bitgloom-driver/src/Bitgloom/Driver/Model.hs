{-# LANGUAGE NoImplicitPrelude          #-}

module Bitgloom.Driver.Model where

import ClassyPrelude.Yesod ((>>))
import Database.Persist.Sql

import qualified Bitgloom.Driver.Model.Configuration as Configuration
import qualified Bitgloom.Driver.Model.Job           as Job

migrate :: Migration
migrate =
  Configuration.migrateConfiguration >> Job.migrateJob
