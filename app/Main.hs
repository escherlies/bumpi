{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import AppM (runAppM)
import qualified Cli
import Config (parseArgs)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString (fromString))
import Data.Text (Text, lines, pack, strip, unlines, unpack)
import Monad.App (MonadApp)
import Monad.Config (Config (bump), MonadConfig (getConfig), Output (..), output)
import Monad.Log (MonadLog, logInfo)
import Monad.Version (MonadVersion)
import System.Environment (getArgs)
import System.Process (readProcess)
import Version (Bump, Version, bump, format, showKeywords)
import Prelude hiding (log)


main :: IO ()
main =
  do
    config <- parseArgs <$> getArgs
    runAppM getNextVersion config


getNextVersion :: (MonadApp m) => m ()
getNextVersion = do
  currentVersion <- getGitVersion
  cfg <- getConfig

  logInfo (pack $ show cfg)

  newVersion <- do
    case cfg.bump of
      Just bumpTo -> Just <$> bumpVersion currentVersion bumpTo
      Nothing -> getNextVersionInteractive currentVersion

  mapM_ (outputVersion cfg.output) newVersion


outputVersion :: (MonadVersion m, MonadIO m) => Output -> Version -> m ()
outputVersion o version =
  do
    formated <- Version.format version
    liftIO $
      case o of
        Cli ->
          Prelude.putStr formated
        File ->
          do
            writeFile "VERSION" formated
            Cli.putStyledLn
              [Cli.fgColor Cli.Green]
              [ "*** Saved to ./VERSION! ***"
              ]


-- | Get latest git version tag
getGitVersion :: MonadIO m => m Version
getGitVersion =
  do
    versionText <- strip <$> readProcessAsText "git" (words "describe --tags --abbrev=0") ""
    --             ^ strip newline

    pure $ fromString $ unpack versionText


getNextVersionInteractive :: (MonadLog m, MonadVersion m, MonadIO m) => Version -> m (Maybe Version)
getNextVersionInteractive currentVersion = do
  -- Get latest commits for the user to review
  commits <- readProcessAsText "bash" ["-c", "git log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"%s\""] ""

  printFriendlyUserMessage currentVersion commits

  -- Ask the user for the bump version arity
  userInput <- askForBumpArity

  -- Handle user input
  case userInput of
    "q" -> do
      void bye
      pure Nothing
    ":q" -> do
      void bye
      pure Nothing
    _ ->
      Just <$> bumpVersion currentVersion (fromString userInput)


bumpVersion :: (MonadVersion m, MonadLog m) => Version -> Bump -> m Version
bumpVersion currentVersion bumpTo =
  do
    -- Actually bump the version!
    let bumped = Version.bump bumpTo currentVersion
    showBumpedMessage currentVersion bumped
    pure bumped


readProcessAsText :: (MonadIO m) => FilePath -> [String] -> String -> m Text
readProcessAsText cmd arg stdin = liftIO $ pack <$> readProcess cmd arg stdin


-- User IO

askForBumpArity :: (MonadLog m, MonadIO m) => m String
askForBumpArity = do
  logInfo $
    Cli.columnLayout
      []
      [ "Bump next version to "
      , Cli.el
          [Cli.dim]
          ("(" <> fromString showKeywords <> ")")
      , ":"
      ]

  liftIO getLine


bye :: Applicative m => m Text
bye = pure $ Cli.layout [Cli.fgColor Cli.Yellow] "Ok, aborting..."


printFriendlyUserMessage :: (MonadVersion m, MonadLog m) => Version -> Text -> m ()
printFriendlyUserMessage currentVersion cs =
  do
    currentVersionText <- pack <$> Version.format currentVersion
    logInfo $
      Cli.columnLayout
        []
        [ "Current version:"
        , Cli.el [Cli.fgColor Cli.Blue] ("  " <> currentVersionText)
        , ""
        , "Commits since last version"
        , Cli.el [] (Data.Text.unlines $ ("  " <>) <$> Data.Text.lines cs)
        ]


showBumpedMessage :: (MonadVersion m, MonadLog m) => Version -> Version -> m ()
showBumpedMessage lastVersion bumpedVersion =
  do
    lastVersionText <- pack <$> Version.format lastVersion
    bumpedVersionText <- pack <$> Version.format bumpedVersion

    -- Clear user input
    logInfo $
      Cli.columnLayout
        []
        [ Cli.moveUp 2
        , Cli.clearLine
        ]

    logInfo $
      Cli.columnLayout
        []
        [ Cli.el [] "Ok! Here is your version:"
        , Cli.el [Cli.fgColor Cli.Red] ("  -" <> lastVersionText)
        , Cli.el [Cli.fgColor Cli.Green] ("  +" <> bumpedVersionText)
        , ""
        ]
