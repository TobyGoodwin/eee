module Check where

import Control.Monad (when)
import Data.List (find)
import Database.Persist (Entity(..))

import Box
import Config
import Database
import Schema

check :: [Test] -> Entity Delivery -> IO ()
check ts ch@(Entity _ d) = do
  let t = find (\x -> boxUnique x == box) $ concatMap destination ts
  case t of
    Nothing -> do
      putStrLn $ "no configuration for " ++ show box
      setArrived ch
    Just t' -> do
      arrvd <- arrived (deliveryKey d) t'
      putStrLn $ deliveryK d ++
                (if arrvd then " " else " not ") ++ "arrived for " ++ show box
      when (arrvd) $ setArrived ch
  return ()
  where
    box = deliveryBox d
