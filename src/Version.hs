{-# LANGUAGE InstanceSigs #-}

module Version where

import Data.String (IsString (fromString))
import GHC.Utils.Misc (split)


data Version
  = Version Int Int Int


data Bump
  = Major
  | Minor
  | Patch


instance IsString Version where
  fromString :: String -> Version
  fromString = parseString


instance IsString Bump where
  fromString :: String -> Bump
  -- Major keywords
  fromString "major" = Major
  fromString "breaking" = Major
  fromString "br" = Major
  fromString "b" = Major
  -- Minor
  fromString "minor" = Minor
  fromString "feature" = Minor
  fromString "feat" = Minor
  fromString "f" = Minor
  -- Patch
  fromString "patch" = Patch
  fromString "fix" = Patch
  fromString "x" = Patch
  fromString other = error $ "Bump keyword " <> other <> " not recognized"


instance Show Version where
  show :: Version -> String
  show (Version major minor patch) = "v" <> show major <> "." <> show minor <> "." <> show patch


parseString :: String -> Version
parseString ('v' : mmp) = parseWithoutPrefix mmp
parseString mmp = parseWithoutPrefix mmp


parseWithoutPrefix :: String -> Version
parseWithoutPrefix mmp = fromList $ read <$> split '.' mmp


fromList :: [Int] -> Version
fromList [major, minor, patch] = Version major minor patch
fromList _ = error "Could not compute. Only [major, minor, patch] :: [Int] supported."


bump :: Bump -> Version -> Version
bump b (Version major minor patch) =
  case b of
    Major ->
      Version (major + 1) 0 0
    Minor ->
      Version major (minor + 1) 0
    Patch ->
      Version major minor (patch + 1)


bumpMajor :: Version -> Version
bumpMajor = bump Major


bumpMinor :: Version -> Version
bumpMinor = bump Minor


bumpPatch :: Version -> Version
bumpPatch = bump Patch
