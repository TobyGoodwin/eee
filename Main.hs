{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Yaml (ParseException, decodeFileEither)
import System.Environment (getArgs)

import Check
import Config
import Database
import Send
import Stack
import Status

main :: IO ()
main = do
  cfg <- decodeFileEither "eee.cfg" :: IO (Either ParseException Config)
  case cfg of
    Left e -> fail $ show e
    Right c -> runReaderT run c

run :: Eee ()
run = do
  args <- liftIO $ getArgs
  case take 1 args of
    ("check":_) -> checkAll
    ("clear":_) -> liftIO clearAll
    ("send":_) -> sendAll
    ("status":_) -> statusAll
    _ -> usage

clearAll :: IO ()
clearAll = do
  cs <- checks
  mapM_ setArrived cs

statusAll :: Eee ()
statusAll = liftIO $ do
  cs <- checks
  -- putStrLn $ "cs is " ++ show cs
  mapM_ status cs

sendAll :: Eee ()
sendAll = do
  defp <- asks defpost
  ts <- asks tests
  -- liftIO $ putStrLn $ "tests is: " ++ show ts
  liftIO $ mapM_ (send defp) ts
  return ()

checkAll :: Eee ()
checkAll = do
  ts <- asks tests
  cs <- liftIO $ checks
  -- liftIO $ putStrLn $ "checks is: " ++ show cs
  liftIO $ mapM_ (check ts) cs

usage :: Eee ()
usage = do
  liftIO $ putStrLn "this is the usage"
