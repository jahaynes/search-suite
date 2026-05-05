{-# LANGUAGE LambdaCase #-}

module Frontier.PoliteStmFrontier ( PoliteStmFrontier
                                  , insertUrl
                                  , new
                                  , popEarliestHostUrl
                                  , signalUrlCompleted
                                  ) where

import Frontier.Class (NextUrl (..), UrlResult (..))
import Restful.Types  (Host, Url, getHost)
import Time.Class     (Millis (..))

import           Control.Concurrent.STM
import           Control.Monad                (unless)
import qualified Data.Heap as H
import           Data.Heap                    (Heap)
import qualified ListT as L
import           StmContainers.Set            (Set)
import qualified StmContainers.Set as S
import           StmContainers.Multimap       (Multimap)
import qualified StmContainers.Multimap as MM

perHostWaitMs :: Int
perHostWaitMs = 50

-- The host->newtime must be set during pop, because it's the only way to access by host
data PoliteStmFrontier =
    PoliteStmFrontier { hostTimes         :: !(TVar (Heap (Millis, Host)))
                      , completedUrls     :: !(Set Url)
                      , remainingHostUrls :: !(Multimap Host Url)
                      }

-- See if it's possible to make this a proper Frontier instance
new :: IO PoliteStmFrontier
new = PoliteStmFrontier <$> newTVarIO mempty
                        <*> S.newIO
                        <*> MM.newIO

-- TODO: use result
signalUrlCompleted :: PoliteStmFrontier -> Url -> UrlResult -> IO ()
signalUrlCompleted frontier url _ = atomically $
    S.insert url (completedUrls frontier)

data HostAction = NoHost
                | WaitMs !Millis
                | HostFound !Host

insertUrl :: PoliteStmFrontier -> Url -> IO ()
insertUrl frontier url = atomically $ do
    done <- S.lookup url (completedUrls frontier)
    unless done $
        case getHost url of
            Nothing ->
                error "No host" -- TODO FIXME
            Just host ->
                MM.insert url host (remainingHostUrls frontier)

popEarliestHostUrl :: PoliteStmFrontier -> Millis -> IO NextUrl
popEarliestHostUrl frontier (Millis now) = atomically $ do

    ht <- readTVar (hostTimes frontier)

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
                    writeTVar (hostTimes frontier) ht'
                    pure $ HostFound host

    -- If a host is ready here, it will have been removed from the heap by this point

    case hostAction of

        NoHost ->

            -- No host-time found
            -- So just grab a host, and register its newtime
            L.head (MM.listT (remainingHostUrls frontier)) >>= \case

                Nothing ->
                    pure NoMoreUrls

                -- Register a later host time and return the Url
                Just (host, url) -> do
                    MM.delete url host (remainingHostUrls frontier)
                    modifyTVar' (hostTimes frontier) (H.insert (Millis (now + perHostWaitMs), host))
                    pure $ NextUrl url

        WaitMs ms ->
            pure $ RetryIn ms

        -- Try to look up a remaining url by host (which is ready now)
        HostFound host ->

            L.head (MM.listTByKey host (remainingHostUrls frontier)) >>= \case

                Nothing ->
                    pure NoMoreUrls

                -- Register a later host time and return the Url
                Just url -> do
                    MM.delete url host (remainingHostUrls frontier)
                    modifyTVar' (hostTimes frontier) (H.insert (Millis (now + perHostWaitMs), host))
                    pure $ NextUrl url
