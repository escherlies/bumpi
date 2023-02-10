{-# LANGUAGE FlexibleContexts #-}

module Monad.Log where


class (Monad m) => MonadLog m where
  getConfig :: m Config


data Config = Config
  { logLevel :: LogLevel
  , silent :: Bool
  }
  deriving (Show)


data LogLevel
  = Info
  | Warn
  deriving (Eq, Ord, Show, Read)
