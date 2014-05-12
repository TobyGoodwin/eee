module Database where

import Database.Persist
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)

import Schema

store :: Text -> Text -> IO ()
store key box = do
  now <- getCurrentTime
  did <- runDB $ insert $ Delivery key box now False
  putStrLn $ "store " ++ show key ++ " " ++ show box ++ " => " ++ show did
