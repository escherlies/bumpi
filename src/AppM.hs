{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module AppM where

import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Monad.App (MonadApp)
import Monad.Config (Config (logger, prefixed), MonadConfig (..))
import Monad.Log (Config, MonadLog (..))
import Monad.Version (MonadVersion (..))
import Prelude hiding (log)


newtype AppM a = AppM (ReaderT Monad.Config.Config IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Monad.Config.Config
    , MonadApp
    )


runAppM :: AppM a -> Monad.Config.Config -> IO a
runAppM (AppM rt) = runReaderT rt


instance MonadConfig AppM where
  getConfig :: AppM Monad.Config.Config
  getConfig = ask


instance MonadVersion AppM where
  getConfig :: AppM Bool
  getConfig = asks Monad.Config.prefixed


instance MonadLog AppM where
  getConfig :: AppM Monad.Log.Config
  getConfig = asks Monad.Config.logger
