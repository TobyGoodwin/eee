{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving,
    TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

module Schema where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Delivery
  key ByteString
  box Text
  time UTCTime
  arrived Bool
  deriving Show
|]

runDB :: SqlPersistM a -> IO a
runDB action =
  runSqlite "eee.sqlite" $ do
    runMigration migrateAll
    action
{-
mig :: SqlPersistM ()
mig = do
  runMigration migrateAll
  michaelId <- insert $ Person "Michael" 26
  michael <- get michaelId
  liftIO $ print michael
  -}
