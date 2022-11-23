module Main where

import Cli (el)
import qualified Cli
import Data.String (IsString (fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import System.Process (readProcess)
import Version (Version, bump, showKeywords)


main :: IO T.Text
main = getNextVersionInteractive


getNextVersionInteractive :: IO T.Text
getNextVersionInteractive = do
  -- Get latest git tag
  v <- T.strip <$> readProcessAsText "git" (words "describe --tags --abbrev=0") ""
  --     ^ strip newline

  -- Get latest commits for the user to review
  commits <- readProcessAsText "bash" ["-c", "git log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"%s\""] ""
  printFriendlyUserMessage v commits

  -- Ask the user for the bump version arity
  userInput <- askForBumpArity

  -- Handle user input
  case userInput of
    "q" -> bye
    ":q" -> bye
    _ ->
      do
        -- Actually bump the version!
        let bumped = bump (fromString userInput) (fromString $ unpack v)
        showBumpedMessage v bumped
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


bye :: IO T.Text
bye = return $ Cli.layout [Cli.fgColor Cli.Yellow] "Ok, aborting..."


printFriendlyUserMessage :: T.Text -> T.Text -> IO ()
printFriendlyUserMessage v cs =
  Cli.putLines
    [ "Current version:"
    , Cli.el [Cli.fgColor Cli.Blue] ("  " <> v)
    , ""
    , "Commits since last version"
    , Cli.el [] (T.unlines $ ("  " <>) <$> T.lines cs)
    ]


showBumpedMessage :: T.Text -> Version -> IO ()
showBumpedMessage v bumped = do
  -- Clear user input
  Cli.putLines
    [ Cli.moveUp 2
    , Cli.clearLine
    ]

  Cli.putLines
    [ el [] "Ok! Here is your version:"
    , el [Cli.fgColor Cli.Red] ("  -" <> v)
    , el [Cli.fgColor Cli.Green] ("  +" <> T.pack (show bumped))
    , ""
    , el [Cli.fgColor Cli.Green] "*** Saved to ./VERSION! ***"
    , ""
    ]
