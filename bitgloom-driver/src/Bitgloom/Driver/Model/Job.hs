{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitgloom.Driver.Model.Job  where

import ClassyPrelude.Yesod

import qualified Data.Bitcoin.Types as BT
import Bitgloom.Driver.Types (Fee)

share [mkPersist sqlSettings, mkMigrate "migrateJob"] [persistLowerCase|
Job
    iterationsLeft Int
    btcLeft        BT.Btc
    percentage     Fee

    deriving Show
|]

-- | Stores a new job inside the database and returns its identifier
store :: (MonadIO m, Functor m)
      => Job                            -- ^ Job we wish to create
      -> ReaderT SqlBackend m (Key Job) -- ^ Returns unique identifier of Job we just created
store = insert
