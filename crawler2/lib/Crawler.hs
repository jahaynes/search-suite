{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs #-}

module Crawler where

import Restful

import Control.Exception.Safe     (MonadCatch, MonadThrow)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Functor               ((<&>))
import Data.Text                  (Text)

data Env = Env Manager

newtype Crawler a =
    Crawler { unCrawler :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

getHttp :: Crawler Manager
getHttp = Crawler (ask <&> \(Env http) -> http)

instance Restful Crawler where

    fetchGet :: String -> Crawler (Either [Text] Response)
    fetchGet url =
        getHttp >>= \http ->
            fetchGetImpl http url
