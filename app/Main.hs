{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import App (AppM, runAppM)
import qualified Cli
import Config (Config (bump), parseArgs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Process (readProcess)
import Version (Bump, Version, bump, showKeywords)


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


getGitVersion :: AppM Version
getGitVersion =
  do
    -- Get latest git tag
    versionText <- liftIO $ T.strip <$> readProcessAsText "git" (words "describe --tags --abbrev=0") ""
    --            ^ strip newline

    pure $ fromString $ unpack versionText


getNextVersionInteractive :: Version -> AppM Text
getNextVersionInteractive currentVersion = do
  -- Get latest commits for the user to review
  commits <- liftIO $ readProcessAsText "bash" ["-c", "git log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"%s\""] ""
  liftIO $ printFriendlyUserMessage (pack $ show currentVersion) commits

  -- Ask the user for the bump version arity
  userInput <- liftIO askForBumpArity

  -- Handle user input
  case userInput of
    "q" -> bye
    ":q" -> bye
    _ -> bumpVersion currentVersion (fromString userInput)


bumpVersion :: (MonadIO m, IsString a) => Version -> Version.Bump -> m a
bumpVersion currentVersion bumpTo =
  liftIO $
    do
      -- Actually bump the version!
      let bumped = Version.bump bumpTo currentVersion
      showBumpedMessage currentVersion bumped
      writeFile "VERSION" (show bumped)
      return $ fromString $ show bumped


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


printFriendlyUserMessage :: T.Text -> T.Text -> IO ()
printFriendlyUserMessage v cs =
  Cli.putLines
    [ "Current version:"
    , Cli.el [Cli.fgColor Cli.Blue] ("  " <> v)
    , ""
    , "Commits since last version"
    , Cli.el [] (T.unlines $ ("  " <>) <$> T.lines cs)
    ]


showBumpedMessage :: Version -> Version -> IO ()
showBumpedMessage lastVersion bumpedVersion = do
  -- Clear user input
  Cli.putLines
    [ Cli.moveUp 2
    , Cli.clearLine
    ]

  Cli.putLines
    [ Cli.el [] "Ok! Here is your version:"
    , Cli.el [Cli.fgColor Cli.Red] ("  -" <> pack (show lastVersion))
    , Cli.el [Cli.fgColor Cli.Green] ("  +" <> pack (show bumpedVersion))
    , ""
    , Cli.el [Cli.fgColor Cli.Green] "*** Saved to ./VERSION! ***"
    , ""
    ]
