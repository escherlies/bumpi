{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module AppM where

import Config (Config (prefixed), MonadConfig, prefixed)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Monad.App (AppConfig (cliCfg, logCfg), MonadApp, MonadConfigApp)
import Monad.Log (Config, MonadLog (getConfig))
import Monad.Version (MonadVersion (..))
import Prelude hiding (log)


newtype AppM a = AppM (ReaderT AppConfig IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadConfig
    , MonadReader AppConfig
    , MonadIO
    , MonadConfigApp
    , MonadApp
    )


runAppM :: AppM a -> AppConfig -> IO a
runAppM (AppM rt) = runReaderT rt


instance MonadVersion AppM where
  getConfig :: AppM Bool
  getConfig = asks (Config.prefixed . cliCfg)


instance MonadLog AppM where
  getConfig :: AppM Monad.Log.Config
  getConfig = asks logCfg
