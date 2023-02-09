module Monad.App where

import Config (MonadConfig)
import Control.Monad.Cont (MonadIO)
import Monad.Version (MonadVersion)


class
  ( Monad a
  , MonadVersion a
  , MonadConfig a
  , MonadIO a
  ) =>
  MonadApp a