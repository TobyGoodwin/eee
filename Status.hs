module Status where

import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Database.Persist (Entity(..))

import Schema

status (Entity _ d) = do
  now <- getCurrentTime
  let t = niceTime $ diffUTCTime now $ deliveryTime d
  putStrLn $ T.unpack (deliveryBox d) ++
                        " missing for " ++ t

niceTime :: NominalDiffTime -> String
niceTime x | x < 100 = overUnit 1 "sec"
           | x < 6000 = overUnit 60 "min"
           | x < 24 * 3600 = overUnit 3600 "hour"
           | otherwise = overUnit 86400 "day"
  where
  overUnit o u = let y = floor (x / o) :: Int
                  in show y ++ " " ++ u
