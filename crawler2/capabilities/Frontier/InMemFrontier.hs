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
import           Data.Heap (Heap)
import qualified Data.Heap as H
import           Data.IORef
import           Data.Set (Set)
import qualified Data.Set as S

data InMemFrontier =
    InMemFrontier { done      :: !(IORef (Set Url))
                  , remaining :: !(IORef (Heap Url))
                  }

new' :: IO InMemFrontier
new' = InMemFrontier <$> newIORef mempty <*> newIORef mempty

insert' :: InMemFrontier -> Url -> IO ()
insert' frontier url = do
    d <- readIORef (done frontier)
    r <- readIORef (remaining frontier)
    unless (S.member url d)
           (writeIORef (remaining frontier) (H.insert url r))

nextUrl' :: InMemFrontier -> IO NextUrl
nextUrl' frontier = do
    x <- readIORef (remaining frontier)
    case H.viewMin x of
        Nothing -> pure NoMoreUrls
        Just (u, us) -> do
            writeIORef (remaining frontier) us
            modifyIORef' (done frontier) (S.insert u)
            pure $ NextUrl u
