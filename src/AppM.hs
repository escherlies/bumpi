{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module AppM where

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Monad.App (MonadApp)
import Monad.Config (Config (logger, prefixed), MonadConfig (..))
import Monad.Log (Level, Log, MonadLog (..), logDefault)
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
  getConfig :: AppM Monad.Log.Log
  getConfig = asks Monad.Config.logger


  logLevel :: Level -> Text -> AppM ()
  logLevel = logDefault


  put :: Text -> AppM ()
  put = liftIO . Data.Text.IO.putStrLn
