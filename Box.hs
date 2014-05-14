{-# LANGUAGE OverloadedStrings #-}

module Box where

import Config
import Connection
import qualified IMAP

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.Text as T
import Network.HaskellNet.POP3
import Network.HaskellNet.POP3.SSL

arrived :: ByteString -> Mailbox -> IO Bool
arrived key box = do
  -- putStrLn $ "going to connect to " ++ show (server box) ++ " with " ++ show (protocol box)
  case protocol box of
    "imap" -> IMAP.arrived Plain key box
    "imaps" -> IMAP.arrived Secure key box
    "pop3s" -> arrivedPop connectPop3SSL key box
    _ -> do
      putStrLn $ "unknown protocol " ++ show (protocol box)
      return True

-- arrivedPop :: ByteString -> Mailbox -> IO Bool
arrivedPop connector key box = do
  p <- connector $ T.unpack $ server box
  user p $ T.unpack $ username box
  pass p $ T.unpack $ password box
  ms <- allList p
  -- putStrLn $ "ms is " ++ show ms
  cs <- mapM (retr p . fst) ms
  closePop3 p
  return $ any (key `S.isInfixOf`) cs
