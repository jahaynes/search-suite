{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Search ( Env (..)
              , Search
              , runSearch
              , throw
              ) where

import Environment     (NewEnvironment (..), Env (..))
import EnvironmentShim
import Exception       (Exception (..))

import           Control.Monad.IO.Class     (MonadIO, liftIO)
--import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except -- (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader -- (ReaderT (..), ask)
import           Data.Text                  (Text)

newtype Search a =
    Search { unSearch :: ExceptT [Text] (ReaderT Env IO) a }
        deriving (Functor, Applicative, Monad, MonadIO) -- MonadFail

-- Does not force, or catch panics
-- Do those as needed within individual capabilities
-- Use throwE to signal failures
runSearch :: Search a -> Env -> IO (Either [Text] a)
runSearch = runReaderT
          . runExceptT
          . unSearch

instance NewEnvironment Search where
    -- getEnv = Search (lift ask)
    getCollectionsPath = liftIO getCollectionsPathImpl
    getIndexerBinary   = liftIO getIndexerBinaryImpl
    getProxySetting    = liftIO getProxySettingImpl

instance Exception Search where
    throw = Search . throwE
