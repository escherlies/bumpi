module Monad.Version where


class Monad m => MonadVersion m where
  -- | The only config atm: Whether or not to prefix
  getConfig :: m Bool
