module Main where

import Cli (el, line)
import qualified Cli
import Data.String (IsString (fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import System.Process (readProcess)
import Version (bump, showKeywords)


main :: IO String
main =
  gnv


gnv :: IO String
gnv = do
  v <- T.strip <$> readProcessT "git" (words "describe --tags --abbrev=0") ""
  --     ^ strip newline

  commits <- readProcessT "bash" ["-c", "git log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"%s\""] ""

  printFriendlyUserMessage v commits
  bt <- getBumpType
  let bumped = bump (fromString bt) (fromString $ unpack v)
  Cli.putLines
    [ el [] "Ok! Here is your version:"
    , line
        []
        [ el [] ("  " <> v <> " -> ")
        , el [Cli.fgColor Cli.Green] (T.pack (show bumped))
        ]
    , ""
    ]
  return (show bumped)


getBumpType :: IO String
getBumpType = do
  Cli.putStyledLn
    []
    [ "Bump next version to "
    , Cli.el
        [Cli.dim]
        ("(" <> fromString showKeywords <> ")")
    , ":"
    ]

  getLine


readProcessT :: FilePath -> [String] -> String -> IO T.Text
readProcessT cmd arg stdin = T.pack <$> readProcess cmd arg stdin


printFriendlyUserMessage :: T.Text -> T.Text -> IO ()
printFriendlyUserMessage v cs =
  Cli.putLines
    [ "Current version:"
    , Cli.el [Cli.fgColor Cli.Yellow] ("  " <> v)
    , ""
    , "Commits since last version"
    , Cli.el [] (T.unlines $ ("  " <>) <$> T.lines cs)
    ]
