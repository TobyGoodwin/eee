{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Text (Text)
import Data.Yaml (ParseException, decodeFileEither)

import Config
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
  defp <- asks defpost
  ts <- asks tests
  liftIO $ putStrLn $ "tests is: " ++ show ts
  liftIO $ mapM (send defp) ts
  return ()

sendAll :: Eee ()
sendAll = do
  defp <- asks defpost
  ts <- asks tests
  liftIO $ mapM_ (send defp) ts
