{-# LANGUAGE NamedFieldPuns #-}

module LogM (logInfo, logWarn, logInfoStyled) where

import qualified Cli
import Control.Monad.Reader (MonadIO (liftIO))
import Data.Text (Text)
import Monad.Log (Config (Config, logLevel, silent), LogLevel (..), MonadLog (getConfig))
import Prelude hiding (log)


logWith :: (MonadLog m, MonadIO m) => LogLevel -> [Cli.Style] -> Text -> m ()
logWith givenLoglevel attrs text =
  do
    (Config {logLevel, silent}) <- getConfig
    if silent || givenLoglevel < logLevel
      then pure ()
      else prettyLog givenLoglevel attrs text


prettyLog :: MonadIO m => LogLevel -> [Cli.Style] -> Text -> m ()
prettyLog loglevel attrs text =
  case loglevel of
    Info ->
      prettyPut Cli.Black attrs text
    Warn ->
      prettyPut Cli.Yellow attrs text


prettyPut :: MonadIO m => Cli.Color -> [Cli.Style] -> Text -> m ()
prettyPut color attrs text = liftIO $ Cli.putStyledLn ([Cli.fgColor color] <> attrs) [Cli.el [] text]


logInfo :: (MonadLog m, MonadIO m) => Text -> m ()
logInfo = logWith Info []


logInfoStyled :: (MonadLog m, MonadIO m) => [Cli.Style] -> Text -> m ()
logInfoStyled = logWith Info


logWarn :: (MonadLog m, MonadIO m) => Text -> m ()
logWarn = logWith Warn []
