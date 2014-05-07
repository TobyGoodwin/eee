{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson.TH
import Data.Text (Text)
import Data.Yaml (ParseException, decodeFileEither)

import Schema

data Mailbox = Mailbox
                { protocol :: Text
                , server :: Text
                , username :: Text
                , password :: Text
                } deriving (Eq, Show)

data Test = Test
                { recipient :: Text
                , destination :: Mailbox
                } deriving (Eq, Show)

data Config = Config
                { test :: Test
                } deriving (Eq, Show)

deriveJSON defaultOptions ''Config
deriveJSON defaultOptions ''Mailbox
deriveJSON defaultOptions ''Test

main :: IO ()
main = do
  cfg <- decodeFileEither "eee.cfg" :: IO (Either ParseException Config)
  case cfg of
    Left e -> fail $ show e
    Right c -> print c
  tryit
