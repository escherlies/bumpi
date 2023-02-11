{-# LANGUAGE FlexibleContexts #-}

module Monad.Log where

import qualified Cli
import Data.Text (Text)
import Prelude hiding (log)


class Monad m => MonadLog m where
  getConfig :: m Log
  log :: Text -> m ()


newtype Log
  = Log Level
  deriving (Show)


data Level
  = Silent
  | Info
  | Warn
  deriving (Eq, Ord, Show, Read)


logWith :: (MonadLog m) => Level -> [Cli.Style] -> Text -> m ()
logWith givenLoglevel attrs text =
  do
    (Log level) <- getConfig
    if givenLoglevel < level
      then pure ()
      else prettyLog givenLoglevel attrs text


prettyLog :: (MonadLog m) => Level -> [Cli.Style] -> Text -> m ()
prettyLog loglevel attrs text =
  case loglevel of
    Silent ->
      pure ()
    Info ->
      prettyPut Cli.Black attrs text
    Warn ->
      prettyPut Cli.Yellow attrs text


prettyPut :: (MonadLog m) => Cli.Color -> [Cli.Style] -> Text -> m ()
prettyPut color attrs text = log $ Cli.layout ([Cli.fgColor color] <> attrs) (Cli.el [] text)


logInfo :: (MonadLog m) => Text -> m ()
logInfo = logWith Info []


logInfoStyled :: (MonadLog m) => [Cli.Style] -> Text -> m ()
logInfoStyled = logWith Info


logWarn :: (MonadLog m) => Text -> m ()
logWarn = logWith Warn []
