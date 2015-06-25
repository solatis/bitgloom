{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitgloom.Driver.Model.Configuration where

import ClassyPrelude.Yesod

share [mkPersist sqlSettings, mkMigrate "migrateConfiguration"] [persistLowerCase|
Configuration
    torPort     Int
    btcHost     Text
    btcPort     Int
    btcUsername Text
    btcPassword Text

    deriving Show
|]

store :: (PersistQuery (PersistEntityBackend s), MonadIO m, Functor m, PersistEntity s)
      => s
      -> ReaderT (PersistEntityBackend s) m ()
store conf = do
  list <- selectList [] [LimitTo 1]

  case list of
   [c] -> replace (entityKey c) conf
   []  -> void (insert conf)
   _   -> error "Database corruption: more than one configuration entry in the Sqlite database!"

retrieve :: (MonadIO m)
         => ReaderT SqlBackend m Configuration
retrieve = do
  list <- selectList [] [LimitTo 1]

  case list of
   [conf] -> return (entityVal conf)
   []     -> insert (Configuration 9151 "127.0.0.1" 8332 "user" "pass") >> retrieve
   _      -> error "Database corruption: more than one configuration entry in the Sqlite database!"
