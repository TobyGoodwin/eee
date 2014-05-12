module Key where

import Crypto.Hash (Digest, SHA256, digestToHexByteString, hash)
import Data.ByteString.Char8 (hGet)
import System.IO (IOMode(ReadMode), withBinaryFile)

genKey = do
  stuff <- withBinaryFile "/dev/urandom" ReadMode $ flip hGet 32
  return $ digestToHexByteString (hash stuff :: Digest SHA256)
