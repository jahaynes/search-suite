{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs,
             LambdaCase #-}

module Frontier.PoliteStmFrontier where

import Frontier.Class (Frontier (..), NextUrl (..), UrlResult (..))
import Restful.Types  (Host, Url, getHost)
import Time.Class     (Millis (..), Time (..))

import           Control.Concurrent.STM
import           Control.Exception.Safe       (MonadCatch, MonadThrow)
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Reader   (ReaderT, ask)
import qualified Data.Heap as H
import           Data.Heap                    (Heap)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import qualified ListT as L
import           StmContainers.Set            (Set)
import qualified StmContainers.Set as S
import           StmContainers.Multimap       (Multimap)
import qualified StmContainers.Multimap as MM

perHostWaitMs :: Int
perHostWaitMs = 50

data Env = Env { hostTimes         :: !(TVar (Heap (Millis, Host)))
               , completedUrls     :: !(Set Url)
               , remainingHostUrls :: !(Multimap Host Url)
               }

newtype PoliteStmFrontier a =
    PoliteStmFrontier { unPoliteStmFrontier :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

data HostAction = NoHost
                | WaitMs !Millis
                | HostFound !Host

new :: IO Env
new = Env <$> newTVarIO mempty
          <*> S.newIO
          <*> MM.newIO

instance Time PoliteStmFrontier where

    currentMillis = Millis . round . (* 1000) <$> liftIO getPOSIXTime

instance Frontier PoliteStmFrontier where

    completed :: Url -> UrlResult -> PoliteStmFrontier ()
    completed url _ = PoliteStmFrontier $ do
        env <- ask
        liftIO $ atomically $
            S.insert url (completedUrls env)

    insert :: Url -> PoliteStmFrontier ()
    insert url = PoliteStmFrontier $ do
        env <- ask
        liftIO $ atomically $ do
            done <- S.lookup url (completedUrls env)
            unless done $
                case getHost url of
                    Nothing ->
                        error "No host" -- TODO FIXME
                    Just host ->
                        MM.insert url host (remainingHostUrls env)

    nextUrl :: PoliteStmFrontier NextUrl
    nextUrl = do
        Millis now <- currentMillis
        PoliteStmFrontier $ do
                    
            env <- ask
            liftIO $ atomically $ do

                ht <- readTVar (hostTimes env)

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
                                writeTVar (hostTimes env) ht'
                                pure $ HostFound host

                -- If a host is ready here, it will have been removed from the heap by this point

                case hostAction of

                    NoHost ->

                        -- No host-time found
                        -- So just grab a host, and register its newtime
                        L.head (MM.listT (remainingHostUrls env)) >>= \case

                            Nothing ->
                                pure NoMoreUrls

                            -- Register a later host time and return the Url
                            Just (host, url) -> do
                                MM.delete url host (remainingHostUrls env)
                                modifyTVar' (hostTimes env) (H.insert (Millis (now + perHostWaitMs), host))
                                pure $ NextUrl url

                    WaitMs ms ->
                        pure $ RetryIn ms

                    -- Try to look up a remaining url by host (which is ready now)
                    HostFound host ->

                        L.head (MM.listTByKey host (remainingHostUrls env)) >>= \case

                            Nothing ->
                                pure NoMoreUrls

                            -- Register a later host time and return the Url
                            Just url -> do
                                MM.delete url host (remainingHostUrls env)
                                modifyTVar' (hostTimes env) (H.insert (Millis (now + perHostWaitMs), host))
                                pure $ NextUrl url
