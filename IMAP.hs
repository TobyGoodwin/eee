module IMAP where

import Control.Exception (tryJust)
import Control.Monad (mfilter)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import System.IO.Error (isUserError)

import Connection
import Config

arrived :: Connection -> ByteString -> Mailbox -> IO Bool
arrived Plain = arrived' connectIMAP
arrived Secure = arrived' connectIMAPSSL

arrived' :: (String -> IO IMAPConnection) -> ByteString -> Mailbox -> IO Bool
arrived' connector key box = do
  s <- tryJust (mfilter isUserError . Just) session
  case s of
    Left e -> do
      putStrLn $ "caught exception: " ++ show e
      return False
    Right r -> return r
  where
    session = do
      p <- connector $ T.unpack $ server box
      login p (T.unpack $ username box) (T.unpack $ password box)
      select p $ fromMaybe "INBOX" (fmap T.unpack $ boxFolder box)
      ms <- search p [BODYs (S.unpack key)]
      -- putStrLn $ "ms is " ++ show ms
      close p
      return $ not $ null ms
