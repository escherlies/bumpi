{-# LANGUAGE FlexibleContexts #-}

module Monad.App where

import Control.Monad.IO.Class (MonadIO)
import Monad.Config (MonadConfig)
import Monad.Log (MonadLog)
import Monad.Version (MonadVersion)


class
  ( Monad a
  , MonadVersion a
  , MonadConfig a
  , MonadLog a
  , MonadIO a
  ) =>
  MonadApp a
