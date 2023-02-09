module App where

import Config (Config)
import Control.Monad.Reader (ReaderT (runReaderT))


type AppM a = ReaderT Config IO a


runAppM :: AppM a -> Config -> IO a
runAppM = runReaderT
