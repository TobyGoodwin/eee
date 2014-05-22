{-# LANGUAGE OverloadedStrings #-}

module Box where

import Config
import Connection
import qualified IMAP
import qualified POP

import Data.ByteString (ByteString)

arrived :: ByteString -> Mailbox -> IO Bool
arrived key box = do
  -- putStrLn $ "going to connect to " ++ show (server box) ++ " with " ++ show (protocol box)
  case protocol box of
    "imap" -> IMAP.arrived Plain key box
    "imaps" -> IMAP.arrived Secure key box
    "pop3" -> POP.arrived Plain key box
    "pop3s" -> POP.arrived Secure key box
    _ -> do
      putStrLn $ "unknown protocol " ++ show (protocol box)
      return True
