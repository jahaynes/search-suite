{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs #-}

module Crawler ( Crawler
               , runCrawler
               ) where

import Restful.Class (Restful (..))
import Restful.IO    (fetchGetImpl)
import Restful.Types (Url, Response)

import Control.Exception.Safe     (MonadCatch, MonadThrow)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Functor               ((<&>))
import Data.Text                  (Text)
import Network.HTTP.Client        (Manager, defaultManagerSettings, newManager)

data Env = Env Manager

newtype Crawler a =
    Crawler { unCrawler :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

getHttp :: Crawler Manager
getHttp = Crawler (ask <&> \(Env http) -> http)

instance Restful Crawler where

    fetchGet :: Url -> Crawler (Either [Text] Response)
    fetchGet url =
        getHttp >>= \http ->
            fetchGetImpl http url

runCrawler :: Crawler a -> IO a
runCrawler crawler = do
    http <- newManager defaultManagerSettings
    runReaderT (unCrawler crawler) (Env http)
