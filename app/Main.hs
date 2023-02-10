{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import AppM (runAppM)
import qualified Cli
import Config (bump, parseArgs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Monad.App (AppConfig (AppConfig, cliCfg, logCfg), MonadApp)
import Monad.Log (Config (..), LogLevel (Info), MonadLog)
import Monad.Version (MonadVersion)
import System.Environment (getArgs)
import System.Process (readProcess)
import Version (Bump, Version, bump, showKeywords, toStringM, toTextM)


main :: IO T.Text
main =
  do
    args <- getArgs
    cliCfg <- parseArgs args
    let logCfg = Monad.Log.Config {silent = False, logLevel = Info}
    runAppM
      getNextVersion
      ( AppConfig
          { cliCfg = cliCfg
          , logCfg = logCfg
          }
      )


getNextVersion :: (MonadApp m) => m Text
getNextVersion = do
  currentVersion <- getGitVersion

  (AppConfig {cliCfg}) <- ask
  case cliCfg.bump of
    Just bumpTo -> bumpVersion currentVersion bumpTo
    Nothing -> getNextVersionInteractive currentVersion


-- | Get latest git version tag
getGitVersion :: MonadIO m => m Version
getGitVersion =
  do
    versionText <- T.strip <$> readProcessAsText "git" (words "describe --tags --abbrev=0") ""
    --            ^ strip newline

    pure $ fromString $ unpack versionText


getNextVersionInteractive :: (MonadLog m, MonadVersion m) => Version -> m Text
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


bumpVersion :: (MonadVersion m, MonadIO m, IsString b) => Version -> Bump -> m b
bumpVersion currentVersion bumpTo =
  do
    versionText <- Version.toStringM currentVersion
    -- Actually bump the version!
    let bumped = Version.bump bumpTo currentVersion

    showBumpedMessage currentVersion bumped
    liftIO $ writeFile "VERSION" versionText
    pure $ fromString versionText


readProcessAsText :: (MonadIO m) => FilePath -> [String] -> String -> m Text
readProcessAsText cmd arg stdin = liftIO $ T.pack <$> readProcess cmd arg stdin


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


printFriendlyUserMessage :: (MonadVersion m, MonadLog m) => Version -> T.Text -> m ()
printFriendlyUserMessage currentVersion cs =
  do
    currentVersionText <- Version.toTextM currentVersion
    liftIO $
      Cli.putLines
        [ "Current version:"
        , Cli.el [Cli.fgColor Cli.Blue] ("  " <> currentVersionText)
        , ""
        , "Commits since last version"
        , Cli.el [] (T.unlines $ ("  " <>) <$> T.lines cs)
        ]


showBumpedMessage :: (MonadVersion m, MonadIO m) => Version -> Version -> m ()
showBumpedMessage lastVersion bumpedVersion =
  do
    lastVersionText <- Version.toTextM lastVersion
    bumpedVersionText <- Version.toTextM bumpedVersion

    liftIO $ do
      -- Clear user input
      Cli.putLines
        [ Cli.moveUp 2
        , Cli.clearLine
        ]

      Cli.putLines
        [ Cli.el [] "Ok! Here is your version:"
        , Cli.el [Cli.fgColor Cli.Red] ("  -" <> lastVersionText)
        , Cli.el [Cli.fgColor Cli.Green] ("  +" <> bumpedVersionText)
        , ""
        , Cli.el [Cli.fgColor Cli.Green] "*** Saved to ./VERSION! ***"
        , ""
        ]
