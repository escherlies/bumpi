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
  , prefixed :: Bool
  }
  deriving (Show)


parseArgs :: [String] -> IO Config
parseArgs args =
  do
    let bumpArg = fromString <$> getValueOfArg "--bump=" args
        prefixVArg = maybe True (read . capitalise) (getValueOfArg "--prefixed=" args)

        config =
          Config
            { bump = bumpArg
            , prefixed = prefixVArg
            }

    print config
    pure config


hasFlag :: Foldable t => String -> t String -> Bool
hasFlag =
  isJust ... getValueOfArg


getValueOfArg :: Foldable t => String -> t String -> Maybe String
getValueOfArg arg = stripPrefix arg <=< Data.List.find (isPrefixOf arg)
