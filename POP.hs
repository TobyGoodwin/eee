module POP 
  ( arrived
  ) where

import Connection
import Config

import Control.Exception (tryJust)
import Control.Monad (mfilter)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Text as T
import Network.HaskellNet.POP3
import Network.HaskellNet.POP3.Connection
import Network.HaskellNet.POP3.SSL
import System.IO.Error (isUserError)

arrived :: Connection -> ByteString -> Mailbox -> IO Bool
arrived Plain = arrived' connectPop3
arrived Secure = arrived' connectPop3SSL

arrived' :: (String -> IO POP3Connection) -> ByteString -> Mailbox -> IO Bool
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
      user p $ T.unpack $ username box
      pass p $ T.unpack $ password box
      ms <- allList p
      -- putStrLn $ "ms is " ++ show ms
      cs <- mapM (retr p . fst) ms
      closePop3 p
      return $ any (key `S.isInfixOf`) cs
