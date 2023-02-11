{-# LANGUAGE FlexibleContexts #-}

module Monad.Log where


class (Monad m) => MonadLog m where
  getConfig :: m Log


newtype Log
  = Log Level
  deriving (Show)


data Level
  = Silent
  | Info
  | Warn
  deriving (Eq, Ord, Show, Read)
