module IMAP where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Maybe (fromMaybe)
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import qualified Data.Text as T

import Connection
import Config

arrived :: Connection -> ByteString -> Mailbox -> IO Bool
arrived Plain = arrived' connectIMAP
arrived Secure = arrived' connectIMAPSSL

arrived' :: (String -> IO IMAPConnection) -> ByteString -> Mailbox -> IO Bool
arrived' connector key box = do
  p <- connector $ T.unpack $ server box
  login p (T.unpack $ username box) (T.unpack $ password box)
  select p $ fromMaybe "INBOX" (fmap T.unpack $ boxFolder box)
  ms <- search p [BODYs (S.unpack key)]
  -- putStrLn $ "ms is " ++ show ms
  close p
  return $ not $ null ms
