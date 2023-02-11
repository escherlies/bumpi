{-# LANGUAGE FlexibleContexts #-}

module Config where

import Control.Monad ((<=<))
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import GHC.Plugins (capitalise)
import Monad.Config (Config (..), Output (File))
import Monad.Log (Level (Info, Silent), Log (Log))
import Utils ((...))
import Prelude hiding (log)


parseArgs :: [String] -> Monad.Config.Config
parseArgs args =
  do
    let bumpArg = fromString <$> getValueOfArg "--bump=" args
        prefixVArg = maybe True (read . capitalise) (getValueOfArg "--prefixed=" args)
        outputArg = maybe File (read . capitalise) (getValueOfArg "--output=" args)

        -- Shorthand for --log-level=silent
        silentArg = hasFlag "--silent" args
        defaultLoglevel = if silentArg then Silent else Info
        logLevelArg = maybe defaultLoglevel (read . capitalise) (getValueOfArg "--log-level=" args)

        config =
          Monad.Config.Config
            { bump = bumpArg
            , prefixed = prefixVArg
            , log = Log logLevelArg
            , output = outputArg
            }

    if silentArg && isNothing bumpArg
      then error "Cannot use silent in cli mode."
      else config


hasFlag :: Foldable t => String -> t String -> Bool
hasFlag =
  isJust ... getValueOfArg


getValueOfArg :: Foldable t => String -> t String -> Maybe String
getValueOfArg arg = stripPrefix arg <=< Data.List.find (isPrefixOf arg)
