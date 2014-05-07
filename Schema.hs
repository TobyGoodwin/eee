{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving,
    TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

module Schema where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name String
  age Int
  deriving Show
Car
  color String
  make String
  model String
  deriving Show
|]

tryit :: IO ()
tryit = runSqlite ":memory:" mig

mig :: SqlPersistM ()
mig = do
  runMigration migrateAll
  michaelId <- insert $ Person "Michael" 26
  michael <- get michaelId
  liftIO $ print michael
