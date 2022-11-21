module Main where

import System.Process (readProcess)
import Text.Printf (printf)

main :: IO ()
main = do
  current_version <- readProcess "git" (words "describe --tags --abbrev=0") ""
  printf "Commits since last version\n"
  printf "Current version: "
  commits <- readProcess "bash" ["-c", "log $(git describe --tags --abbrev=0)..HEAD --oneline --pretty=\"    %s\""] ""
  printf commits
  print current_version
  return ()
