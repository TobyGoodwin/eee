module Database where

import Database.Persist
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)

import Schema

store :: ByteString -> Text -> IO ()
store key box = do
  now <- getCurrentTime
  runDB $ insert_ $ Delivery key box now False
  putStrLn $ "expect " ++ shorten key ++ " for " ++ show box

checks :: IO [Entity Delivery]
checks = runDB $ selectList [DeliveryArrived !=. True] []

setArrived :: Entity Delivery -> IO ()
setArrived (Entity k _) = runDB $ update k [DeliveryArrived =. True]
