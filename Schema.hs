{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving,
    TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

module Schema where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Delivery
  key ByteString
  box Text
  time UTCTime
  arrived Bool
  deriving Show
|]

-- A deliveryK is a short Key
deliveryK :: Delivery -> String
deliveryK = shorten . deliveryKey
shorten :: ByteString -> String
shorten k = S.unpack (S.take 12 k) ++ "..."

runDB :: SqlPersistM a -> IO a
runDB action =
  runSqlite "eee.sqlite" $ do
    runMigration migrateAll
    action
