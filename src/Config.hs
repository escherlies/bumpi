{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Config where

import Control.Monad ((<=<))
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Maybe (isJust)
import Data.String (fromString)
import GHC.Plugins (capitalise)
import Utils ((...))
import Version (Bump)


data Config = Config
  { bump :: Maybe Bump
  , prefixV :: Bool
  }
  deriving (Show)


parseArgs :: [String] -> IO Config
parseArgs args =
  do
    let bumpArg = fromString <$> getValueOfArg "--bump=" args
        prefixVArg = maybe True (read . capitalise) (getValueOfArg "--prefix-v=" args)

        config =
          Config
            { bump = bumpArg
            , prefixV = prefixVArg
            }

    print config
    pure config


hasFlag :: Foldable t => String -> t String -> Bool
hasFlag =
  isJust ... getValueOfArg


getValueOfArg :: Foldable t => String -> t String -> Maybe String
getValueOfArg arg = stripPrefix arg <=< Data.List.find (isPrefixOf arg)
