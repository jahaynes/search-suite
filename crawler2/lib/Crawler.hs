{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs #-}

module Crawler ( Crawler
               , runCrawler
               ) where

import Frontier.Class          (Frontier (..), NextUrl (..))
import Frontier.InMemFrontier
import Multithread.Class       (Multithread (..))
import Restful.Class           (Restful (..))
import Restful.IO              (fetchGetImpl)
import Restful.Types           (Url, Response)

import qualified Control.Concurrent.Async as A
import           Control.Exception.Safe     (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Text                  (Text)
import           Network.HTTP.Client        (Manager, defaultManagerSettings, newManager)

data Env = Env InMemFrontier Manager

newtype Crawler a =
    Crawler { unCrawler :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance Restful Crawler where

    fetchGet :: Url -> Crawler (Either [Text] Response)
    fetchGet url = do
        Env _ http <- Crawler ask
        fetchGetImpl http url

instance Multithread Crawler where

    mapConcurrently :: Traversable t => (a -> Crawler b) -> t a -> Crawler (t b)
    mapConcurrently f xs =
        Crawler $ do
            env <- ask
            liftIO $ A.mapConcurrently (go env) xs
        where
        go env x = unlift $ f x
            where
            unlift :: Crawler b -> IO b
            unlift (Crawler run) = runReaderT run env
 
instance Frontier Crawler where

    insert :: Url -> Crawler ()
    insert url = do
        Env frontier _ <- Crawler ask
        liftIO $ insert' frontier url

    nextUrl :: Crawler NextUrl
    nextUrl = do
        Env frontier _ <- Crawler ask
        liftIO $ nextUrl' frontier

runCrawler :: Crawler a -> IO a
runCrawler crawler = do
    frontier <- new'
    http <- newManager defaultManagerSettings
    runReaderT (unCrawler crawler) (Env frontier http)
