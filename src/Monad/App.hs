module Monad.App where

import Config (MonadConfig)
import Monad.Version (MonadVersion)


class
  ( Monad a
  , MonadVersion a
  , MonadConfig a
  ) =>
  MonadApp a