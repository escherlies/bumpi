module Monad.Config where

import qualified Monad.Log
import Version (Bump)


class Monad m => MonadConfig m where
  getConfig :: m Config


data Config = Config
  { bump :: Maybe Bump
  , prefixed :: Bool
  , log :: Monad.Log.Log
  , output :: Output
  }
  deriving (Show)


data Output
  = File
  | Cli
  deriving (Read, Show)
