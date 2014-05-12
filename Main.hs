{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.List (find)
import Data.Text (Text)
import Data.Yaml (ParseException, decodeFileEither)
import Database.Persist

import Box
import Config
import Database
import Schema
import Send

-- The monad stack
type Eee = ReaderT Config IO

main :: IO ()
main = do
  cfg <- decodeFileEither "eee.cfg" :: IO (Either ParseException Config)
  case cfg of
    Left e -> fail $ show e
    Right c -> runReaderT run c

run :: Eee ()
run = do
  sendAll
  checkAll

sendAll :: Eee ()
sendAll = do
  defp <- asks defpost
  ts <- asks tests
  liftIO $ putStrLn $ "tests is: " ++ show ts
  liftIO $ mapM_ (send defp) ts
  return ()

checkAll :: Eee ()
checkAll = do
  cs <- liftIO $ checks
  liftIO $ putStrLn $ "checks is: " ++ show cs
  mapM_ check cs
  return ()

check :: Entity Delivery -> Eee ()
check (Entity k d) = do
  ts <- asks tests
  liftIO $ putStrLn $ "delivery " ++ show (deliveryKey d) ++ " dest " ++ show box
  let t = find (\x -> boxUnique x == box) $ concatMap destination ts
  case t of
    Nothing -> do
      liftIO $ putStrLn $ "no configuration for " ++ show box
      liftIO $ runDB $ update k [DeliveryArrived =. True]
    Just t' -> do
      arrvd <- liftIO $ arrived (deliveryKey d) t'
      if (arrvd)
        then do
          liftIO $ putStrLn $ show (deliveryKey d) ++ " arrived for " ++ show box
          liftIO $ runDB $ update k [DeliveryArrived =. True]
        else
          liftIO $ putStrLn $ show (deliveryKey d) ++ " not yet arrived for " ++ show box
  return ()
  where
    box = deliveryBox d
