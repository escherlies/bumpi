{-# LANGUAGE FlexibleContexts #-}

module Monad.Log where

import qualified Cli
import Data.Text (Text)
import Prelude hiding (log)


class Monad m => MonadLog m where
  getConfig :: m Log
  logLevel :: Level -> Text -> m ()
  put :: Text -> m ()


newtype Log
  = Log Level
  deriving (Show)


data Level
  = Debug -- Everything
  | Info -- Info and warnings
  | Warn -- Only warnings
  | Silent -- Nothing
  deriving (Eq, Ord, Show, Read)


logDefault :: MonadLog m => Level -> Text -> m ()
logDefault = logWithSytle []


logWithSytle :: (MonadLog m) => [Cli.Style] -> Level -> Text -> m ()
logWithSytle attrs givenLoglevel text =
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
    Debug ->
      prettyPut Cli.Blue attrs text


prettyPut :: MonadLog m => Cli.Color -> [Cli.Style] -> Text -> m ()
prettyPut color attrs text = put $ Cli.layout ([Cli.fgColor color] <> attrs) (Cli.el [] text)


logInfo :: MonadLog m => Text -> m ()
logInfo = logLevel Info


logInfoStyled :: (MonadLog m) => [Cli.Style] -> Text -> m ()
logInfoStyled s = logWithSytle s Info


logWarn :: (MonadLog m) => Text -> m ()
logWarn = logLevel Warn
