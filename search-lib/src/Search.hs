{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Search ( Env (..)
              , Search
              , runSearch
              , throw
              ) where

import Capability.Environment (Environment (..), Env (..))
import Capability.Exception   (Exception (..))

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT (..), ask)

newtype Search a =
    Search { unSearch :: ExceptT [String] (ReaderT Env IO) a }
        deriving (Functor, Applicative, Monad)
      --  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

-- Does not force, or catch panics
-- Do those as needed within individual capabilities
-- Use throwE to signal failures
runSearch :: Search a -> Env -> IO (Either [String] a)
runSearch = runReaderT
          . runExceptT
          . unSearch

instance Environment Search where
    getEnv = Search (lift ask)

instance Exception Search where
    throw = Search . throwE