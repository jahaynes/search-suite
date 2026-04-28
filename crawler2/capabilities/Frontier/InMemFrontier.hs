{-# LANGUAGE InstanceSigs #-}

-- TODO - implementation, not capability.  Belongs in lib?

module Frontier.InMemFrontier ( InMemFrontier (..)
                              , insert'
                              , nextUrl'
                              , new'
                              ) where

import Frontier.Class
import Restful.Types  (Url)

import           Control.Monad (unless)
import           Data.IORef
import           Data.Set (Set)
import qualified Data.Set as S

data InMemFrontier =
    InMemFrontier { done      :: !(IORef (Set Url))
                  , remaining :: !(IORef (Set Url))
                  }

new' :: IO InMemFrontier
new' = InMemFrontier <$> newIORef mempty <*> newIORef mempty

insert' :: InMemFrontier -> Url -> IO ()
insert' frontier url = do
    d <- readIORef (done frontier)
    r <- readIORef (remaining frontier)
    unless (S.member url d)
           (writeIORef (remaining frontier) (S.insert url r))

nextUrl' :: InMemFrontier -> IO NextUrl
nextUrl' frontier = do
    x <- readIORef (remaining frontier)
    case S.minView x of
        Nothing -> pure NoMoreUrls
        Just (u, us) -> do
            writeIORef (remaining frontier) us
            modifyIORef' (done frontier) (S.insert u)
            pure $ NextUrl u
