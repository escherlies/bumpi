{-# LANGUAGE FlexibleContexts #-}

module Monad.App where

import Config (Config, MonadConfig)
import Control.Monad.Reader (MonadReader)
import Monad.Log (Config, MonadLog)
import Monad.Version (MonadVersion)


class
  ( Monad a
  , MonadVersion a
  , MonadConfig a
  , MonadLog a
  , MonadConfigApp a
  ) =>
  MonadApp a


class MonadReader AppConfig m => MonadConfigApp m


data AppConfig = AppConfig
  { cliCfg :: Config.Config
  , logCfg :: Monad.Log.Config
  }
