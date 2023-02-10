module Monad.Log where

import Control.Monad.IO.Class (MonadIO)


class (MonadIO m) => MonadLog m where
  getConfig :: m Config


data Config = Config
  { logLevel :: LogLevel
  , silent :: Bool
  }


data LogLevel
  = Info
  | Warn
  deriving (Eq, Ord)
