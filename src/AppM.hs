{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module AppM where

import Config (Config (prefixed), MonadConfig, prefixed)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Monad.App (MonadApp)
import Monad.Version (MonadVersion (..))


newtype AppM a = AppM (ReaderT Config IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Config
    , MonadConfig
    , MonadApp
    , MonadIO
    )


runAppM :: AppM a -> Config -> IO a
runAppM (AppM rt) = runReaderT rt


instance MonadVersion AppM where
  getConfig :: AppM Bool
  getConfig = asks Config.prefixed