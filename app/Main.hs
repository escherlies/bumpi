{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import App (AppM, runAppM)
import qualified Cli
import Config (Config (bump, prefixed), parseArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Process (readProcess)
import Version (Bump, Version, bump, showKeywords, toStringM, toTextM)


main :: IO T.Text
main =
  do
    args <- getArgs
    config <- parseArgs args
    runAppM getNextVersion config


getNextVersion :: AppM Text
getNextVersion = do
  currentVersion <- getGitVersion

  config <- ask
  case config.bump of
    Just bumpTo -> bumpVersion currentVersion bumpTo
    Nothing -> getNextVersionInteractive currentVersion


-- | Get latest git version tag
getGitVersion :: AppM Version
getGitVersion =
  do
    versionText <- liftIO $ T.strip <$> readProcessAsText "git" (words "describe --tags --abbrev=0") ""
    --            ^ strip newline

    pure $ fromString $ unpack versionText


getNextVersionInteractive :: Version -> AppM Text
getNextVersionInteractive currentVersion = do
  -- Get latest commits for the user to review
  commits <- liftIO $ readProcessAsText "bash" ["-c", "git log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"%s\""] ""

  printFriendlyUserMessage currentVersion commits

  -- Ask the user for the bump version arity
  userInput <- liftIO askForBumpArity

  -- Handle user input
  case userInput of
    "q" -> bye
    ":q" -> bye
    _ -> bumpVersion currentVersion (fromString userInput)


bumpVersion :: IsString b => Version -> Bump -> AppM b
bumpVersion currentVersion bumpTo =
  do
    versionText <- Version.toStringM currentVersion
    -- Actually bump the version!
    let bumped = Version.bump bumpTo currentVersion

    showBumpedMessage currentVersion bumped
    liftIO $ writeFile "VERSION" versionText
    pure $ fromString versionText


readProcessAsText :: FilePath -> [String] -> String -> IO T.Text
readProcessAsText cmd arg stdin = T.pack <$> readProcess cmd arg stdin


-- User IO

askForBumpArity :: IO String
askForBumpArity = do
  Cli.putStyledLn
    []
    [ "Bump next version to "
    , Cli.el
        [Cli.dim]
        ("(" <> fromString showKeywords <> ")")
    , ":"
    ]

  getLine


bye :: AppM T.Text
bye = pure $ Cli.layout [Cli.fgColor Cli.Yellow] "Ok, aborting..."


printFriendlyUserMessage :: Version -> T.Text -> AppM ()
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


showBumpedMessage :: Version -> Version -> AppM ()
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
