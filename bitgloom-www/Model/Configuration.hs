module Model.Configuration (store, retrieve) where

import ClassyPrelude.Yesod

import Model

store :: (PersistQuery (PersistEntityBackend s), MonadIO m, PersistEntity s)
      => s
      -> ReaderT (PersistEntityBackend s) m ()
store conf = do
  list <- selectList [] [LimitTo 1]

  case list of
   [c] -> replace (entityKey c) conf
   []  -> (insert $ conf) >> return ()
   _   -> error "Database corruption: more than one configuration entry in the Sqlite database!"

retrieve :: (MonadIO m)
         => ReaderT SqlBackend m Configuration
retrieve = do
  list <- selectList [] [LimitTo 1]

  case list of
   [conf] -> return (entityVal conf)
   []     -> (insert $ Configuration "127.0.0.1" 7656 "127.0.0.1" 7655 "127.0.0.1" 8332 "user" "pass") >> retrieve
   _      -> error "Database corruption: more than one configuration entry in the Sqlite database!"
