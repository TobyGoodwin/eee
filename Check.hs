module Check where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (find)
import Database.Persist ((=.), Entity(..), update)

import Box
import Config
import Schema
import Stack

check :: Entity Delivery -> Eee ()
check (Entity k d) = do
  ts <- asks tests
  let t = find (\x -> boxUnique x == box) $ concatMap destination ts
  case t of
    Nothing -> do
      liftIO $ putStrLn $ "no configuration for " ++ show box
      liftIO $ runDB $ update k [DeliveryArrived =. True]
    Just t' -> do
      arrvd <- liftIO $ arrived (deliveryKey d) t'
      liftIO $ putStrLn $ deliveryK d ++
                (if arrvd then " " else " not ") ++ "arrived for " ++ show box
      when (arrvd) $ liftIO $ runDB $ update k [DeliveryArrived =. True]
  return ()
  where
    box = deliveryBox d
