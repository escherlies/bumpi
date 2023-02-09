{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Version where

import Control.Monad.RWS (MonadReader (ask))
import Data.List (intersperse)
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import GHC.Records (HasField (getField))
import GHC.Utils.Misc (split)


data Version
  = Version Int Int Int


toString :: Version -> String
toString (Version major minor patch) =
  show major <> "." <> show minor <> "." <> show patch


toStringPrefixed :: Bool -> Version -> String
toStringPrefixed prefixed =
  if prefixed
    then addPrefix . toString
    else toString


addPrefix :: String -> String
addPrefix = ("v" <>)


toStringM :: (MonadReader config m, HasField "prefixed" config Bool) => Version -> m String
toStringM version =
  do
    prefixed <- getPrefixedConfig
    pure $ toStringPrefixed prefixed version


toTextM :: (MonadReader config m, HasField "prefixed" config Bool) => Version -> m Text
toTextM = fmap pack . toStringM


getPrefixedConfig :: (MonadReader config m, HasField "prefixed" config Bool) => m Bool
getPrefixedConfig = do
  c <- ask
  pure $ getField @"prefixed" c


data Bump
  = Major
  | Minor
  | Patch
  deriving (Show)


instance IsString Version where
  fromString :: String -> Version
  fromString = dropPrefix


instance IsString Bump where
  fromString :: String -> Bump
  fromString s
    | s `elem` majorKeywords = Major
    | s `elem` minorKeywords = Minor
    | s `elem` patchKeywords = Patch
    | otherwise = error $ "Bump keyword " <> s <> " not recognized"


showKeywords :: String
showKeywords =
  foldl (<>) "" $ intersperse ", " $ concat [majorKeywords, minorKeywords, patchKeywords]


majorKeywords :: [String]
majorKeywords = ["major", "breaking", "br", "b", "!"]


minorKeywords :: [String]
minorKeywords = ["minor", "feature", "feat", "f"]


patchKeywords :: [String]
patchKeywords = ["patch", "fix", "x"]


dropPrefix :: String -> Version
dropPrefix ('v' : mmp) = parseString mmp
dropPrefix mmp = parseString mmp


parseString :: String -> Version
parseString mmp = fromList $ read <$> split '.' mmp


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
