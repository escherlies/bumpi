{-# LANGUAGE FlexibleContexts #-}

module Config where

import Control.Monad ((<=<))
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Maybe (isJust)
import Data.String (fromString)
import GHC.Plugins (capitalise)
import Monad.Config (Config (..))
import Monad.Log (Config (Config, logLevel, silent), LogLevel (Info))
import Utils ((...))


parseArgs :: [String] -> Monad.Config.Config
parseArgs args =
  do
    let bumpArg = fromString <$> getValueOfArg "--bump=" args
        prefixVArg = maybe True (read . capitalise) (getValueOfArg "--prefixed=" args)
        silentArg = hasFlag "--silent" args
        logLevelArg = maybe Info (read . capitalise) (getValueOfArg "--log-level=" args)

        config =
          Monad.Config.Config
            { bump = bumpArg
            , prefixed = prefixVArg
            , logger =
                Monad.Log.Config
                  { silent = silentArg
                  , logLevel = logLevelArg
                  }
            }
    config


hasFlag :: Foldable t => String -> t String -> Bool
hasFlag =
  isJust ... getValueOfArg


getValueOfArg :: Foldable t => String -> t String -> Maybe String
getValueOfArg arg = stripPrefix arg <=< Data.List.find (isPrefixOf arg)
