{-# LANGUAGE FlexibleContexts #-}

module Monad.Log where

import Data.Text (Text)


class Monad m => MonadLog m where
  getConfig :: m Log
  log :: Text -> m ()


newtype Log
  = Log Level
  deriving (Show)


data Level
  = Silent
  | Info
  | Warn
  deriving (Eq, Ord, Show, Read)
