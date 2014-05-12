{-# LANGUAGE OverloadedStrings #-}

module Send where

import Prelude hiding ((++))

import ClassyPrelude ((++))
import qualified Data.ByteString.Char8 as S
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HaskellNet.SMTP

import Config
import Database
import Key

send defp t = do
  let p = fromMaybe defp (post t)
  key <- genKey
  let srv = T.unpack $ postServer p
      frm = T.unpack $ postSender p
      to = [ T.unpack $ recipient t ]
      msg = S.pack ("From: " ++ frm ++ "\r\n" ++
                     "To: " ++ head to ++ "\r\n" ++
                     "Subject: EEE Test - ") ++ key ++ "\r\n" ++
                     "\r\n" ++
                     key ++ "\r\n"
  putStrLn $ "sending " ++ show msg ++ " from " ++ show frm ++ " to " ++ show to ++ " via " ++ show srv
  -- XXX not really any good, as all we get back for any failure is "user error
  -- (sendMail error)"
  doSMTP srv (sendMail frm to msg)
  mapM_ (\x -> store key (boxUnique x)) (destination t)
