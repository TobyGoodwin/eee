{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Prelude hiding ((++))

import ClassyPrelude ((++))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Yaml (ParseException, decodeFileEither)

data Mailbox = Mailbox
                { protocol :: Text
                , server :: Text
                , username :: Text
                , password :: Text
                , boxFolder :: Maybe Text
                , boxId :: Maybe Text
                } deriving (Eq, Show)

boxUnique :: Mailbox -> Text
boxUnique x = fromMaybe (username x ++ "@" ++ server x) (boxId x)

data Post = Post
                { postSender :: Text
                , postServer :: Text
                } deriving (Eq, Show)

data Test = Test
                { recipient :: Text
                , destination :: [Mailbox]
                , post :: Maybe Post
                } deriving (Eq, Show)

data Config = Config
                { defpost :: Post
                , tests :: [Test]
                } deriving (Eq, Show)

deriveJSON defaultOptions ''Config
deriveJSON defaultOptions ''Mailbox
deriveJSON defaultOptions ''Post
deriveJSON defaultOptions ''Test
