{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs,
             LambdaCase #-}

module Frontier.PoliteStmFrontier where

import Frontier.Class (Frontier (..), NextUrl (..))
import Restful.Types  (Host, Url, getHost)
import Time.Class     (Millis (..))

import           Control.Concurrent.STM
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Heap as H
import           Data.Heap                    (Heap)
import qualified ListT as L
import           StmContainers.Set            (Set)
import qualified StmContainers.Set as S
import           StmContainers.Multimap       (Multimap)
import qualified StmContainers.Multimap as MM

perHostWaitMs :: Int
perHostWaitMs = 50

data PoliteStmFrontier =
    PoliteStmFrontier { hostTimes         :: !(TVar (Heap (Millis, Host)))
                      , completedUrls     :: !(Set Url)
                      , remainingHostUrls :: !(Multimap Host Url) 
                      }

data HostAction = NoHost
                | WaitMs !Millis
                | HostFound !Host

instance Frontier PoliteStmFrontier where

    newFrontier =
        PoliteStmFrontier <$> newTVarIO mempty
                          <*> S.newIO
                          <*> MM.newIO

    completed p url _ = liftIO . atomically $
        S.insert url (completedUrls p)

    insert p url = liftIO . atomically $ do
        done <- S.lookup url (completedUrls p)
        unless done $
            case getHost url of
                Nothing ->
                    error "No host" -- TODO FIXME
                Just host ->
                    MM.insert url host (remainingHostUrls p)

    nextUrl p (Millis now) = liftIO . atomically $ do

        ht <- readTVar (hostTimes p)

        hostAction <-

            case H.viewMin ht of

                Nothing ->

                    pure NoHost

                Just ((Millis t, host), ht')

                    -- Still too soon for the *earliest* host
                    -- Do nothing and return a wait signal
                    | now < t -> let wait = Millis (t - now)
                                 in pure $ WaitMs wait

                    -- Host established, update the host times and return it
                    | otherwise -> do
                        writeTVar (hostTimes p) ht'
                        pure $ HostFound host

        -- If a host is ready here, it will have been removed from the heap by this point

        case hostAction of

            NoHost ->

                -- No host-time found
                -- So just grab a host, and register its newtime
                L.head (MM.listT (remainingHostUrls p)) >>= \case

                    Nothing ->
                        pure NoMoreUrls

                    -- Register a later host time and return the Url
                    Just (host, url) -> do
                        MM.delete url host (remainingHostUrls p)
                        modifyTVar' (hostTimes p) (H.insert (Millis (now + perHostWaitMs), host))
                        pure $ NextUrl url

            WaitMs ms ->
                pure $ RetryIn ms

            -- Try to look up a remaining url by host (which is ready now)
            HostFound host ->

                L.head (MM.listTByKey host (remainingHostUrls p)) >>= \case

                    Nothing ->
                        pure NoMoreUrls

                    -- Register a later host time and return the Url
                    Just url -> do
                        MM.delete url host (remainingHostUrls p)
                        modifyTVar' (hostTimes p) (H.insert (Millis (now + perHostWaitMs), host))
                        pure $ NextUrl url
