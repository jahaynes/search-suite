module Frontier.StmFrontier ( StmFrontier (..)
                            , insert'
                            , nextUrl'
                            , new'
                            ) where

import Frontier.Class
import Restful.Types  (Url)

import           Control.Concurrent.STM
import           Control.Monad (unless)
import           Data.Set (Set)
import qualified Data.Set as S

data StmFrontier =
    StmFrontier { done      :: !(TVar (Set Url))
                , remaining :: !(TVar (Set Url))
                }

new' :: IO StmFrontier
new' = StmFrontier <$> newTVarIO mempty <*> newTVarIO mempty

insert' :: StmFrontier -> Url -> IO ()
insert' frontier url = atomically $ do
    d <- readTVar (done frontier)
    r <- readTVar (remaining frontier)
    unless (S.member url d)
           (writeTVar (remaining frontier) (S.insert url r))

nextUrl' :: StmFrontier -> IO NextUrl
nextUrl' frontier = atomically $ do
    x <- readTVar (remaining frontier)
    case S.minView x of
        Nothing -> pure NoMoreUrls
        Just (u, us) -> do
            writeTVar (remaining frontier) us
            modifyTVar' (done frontier) (S.insert u)
            pure $ NextUrl u
