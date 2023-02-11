module LogM (logInfo, logWarn, logInfoStyled) where

import qualified Cli
import Control.Monad.Reader (MonadIO (liftIO))
import Data.Text (Text)
import Monad.Log (Level (..), Log (Log), MonadLog (getConfig))
import Prelude hiding (log)


logWith :: (MonadLog m, MonadIO m) => Level -> [Cli.Style] -> Text -> m ()
logWith givenLoglevel attrs text =
  do
    (Log level) <- getConfig
    if givenLoglevel < level
      then pure ()
      else prettyLog givenLoglevel attrs text


prettyLog :: MonadIO m => Level -> [Cli.Style] -> Text -> m ()
prettyLog loglevel attrs text =
  case loglevel of
    Silent ->
      pure ()
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
