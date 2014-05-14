module Stack where

import Control.Monad.Reader (ReaderT)

import Config

-- Our monad stack
type Eee = ReaderT Config IO

