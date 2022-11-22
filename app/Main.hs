module Main where

import qualified Cli
import Data.String (IsString (fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Process (readProcess)
import Version (bump)


main :: IO ()
main =
  gnv >>= putStr


gnv :: IO String
gnv = do
  v <- T.strip <$> readProcessT "git" (words "describe --tags --abbrev=0") ""
  --                 ^ strip newline
  commits <- readProcessT "bash" ["-c", "git log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"    %s\""] ""
  printFriendlyUserMessage v commits
  bt <- getBumpType
  let bumped = bump (fromString bt) (fromString $ unpack v)
  return (show bumped)


getBumpType :: IO String
getBumpType = do
  T.putStrLn "Bump next version to (i):"
  T.putStrLn (Cli.applyStyles (Cli.el [Cli.dim] "(i) Input major, minor, patch or breaking, feature, fix"))
  getLine


readProcessT :: FilePath -> [String] -> String -> IO T.Text
readProcessT cmd arg stdin = T.pack <$> readProcess cmd arg stdin


printFriendlyUserMessage :: T.Text -> T.Text -> IO ()
printFriendlyUserMessage v cs =
  do
    T.putStrLn "Commits since last version"
    T.putStrLn cs
    T.putStrLn "Current version:"
    T.putStrLn v
    return ()
