{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import AppM (runAppM)
import Cli (columnLayout, layout)
import qualified Cli
import Config (parseArgs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString (fromString))
import Data.Text (Text, lines, pack, strip, unlines, unpack)
import LogM (logInfo)
import Monad.App (MonadApp)
import Monad.Config (Config (bump), MonadConfig (getConfig))
import Monad.Log (MonadLog, log)
import Monad.Version (MonadVersion)
import System.Environment (getArgs)
import System.Process (readProcess)
import Version (Bump, Version, bump, showKeywords, toStringM, toTextM)
import Prelude hiding (log)


main :: IO Text
main =
  do
    config <- parseArgs <$> getArgs
    runAppM getNextVersion config


getNextVersion :: (MonadApp m) => m Text
getNextVersion = do
  currentVersion <- getGitVersion
  cfg <- getConfig

  logInfo (pack $ show cfg)

  case cfg.bump of
    Just bumpTo -> bumpVersion currentVersion bumpTo
    Nothing -> getNextVersionInteractive currentVersion


-- | Get latest git version tag
getGitVersion :: MonadIO m => m Version
getGitVersion =
  do
    versionText <- strip <$> readProcessAsText "git" (words "describe --tags --abbrev=0") ""
    --             ^ strip newline

    pure $ fromString $ unpack versionText


getNextVersionInteractive :: (MonadLog m, MonadVersion m, MonadIO m) => Version -> m Text
getNextVersionInteractive currentVersion = do
  -- Get latest commits for the user to review
  commits <- readProcessAsText "bash" ["-c", "git log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"%s\""] ""

  printFriendlyUserMessage currentVersion commits

  -- Ask the user for the bump version arity
  userInput <- askForBumpArity

  -- Handle user input
  case userInput of
    "q" -> bye
    ":q" -> bye
    _ -> bumpVersion currentVersion (fromString userInput)


bumpVersion :: (MonadVersion m, IsString b, MonadLog m, MonadIO m) => Version -> Bump -> m b
bumpVersion currentVersion bumpTo =
  do
    versionText <- Version.toStringM currentVersion
    -- Actually bump the version!
    let bumped = Version.bump bumpTo currentVersion

    showBumpedMessage currentVersion bumped
    liftIO $ writeFile "VERSION" versionText
    pure $ fromString versionText


readProcessAsText :: (MonadIO m) => FilePath -> [String] -> String -> m Text
readProcessAsText cmd arg stdin = liftIO $ pack <$> readProcess cmd arg stdin


-- User IO

askForBumpArity :: MonadIO m => m String
askForBumpArity = liftIO $ do
  Cli.putStyledLn
    []
    [ "Bump next version to "
    , Cli.el
        [Cli.dim]
        ("(" <> fromString showKeywords <> ")")
    , ":"
    ]

  getLine


bye :: Applicative m => m Text
bye = pure $ Cli.layout [Cli.fgColor Cli.Yellow] "Ok, aborting..."


printFriendlyUserMessage :: (MonadVersion m, MonadLog m) => Version -> Text -> m ()
printFriendlyUserMessage currentVersion cs =
  do
    currentVersionText <- Version.toTextM currentVersion
    log $
      columnLayout
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
    lastVersionText <- Version.toTextM lastVersion
    bumpedVersionText <- Version.toTextM bumpedVersion

    -- Clear user input
    log $
      columnLayout
        []
        [ Cli.moveUp 2
        , Cli.clearLine
        ]

    log $
      columnLayout
        []
        [ Cli.el [] "Ok! Here is your version:"
        , Cli.el [Cli.fgColor Cli.Red] ("  -" <> lastVersionText)
        , Cli.el [Cli.fgColor Cli.Green] ("  +" <> bumpedVersionText)
        , ""
        , Cli.el [Cli.fgColor Cli.Green] "*** Saved to ./VERSION! ***"
        , ""
        ]
