{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs,
             LambdaCase,
             OverloadedLists #-}

module Crawler.MultiCrawler ( MultiCrawler
                            , runCrawler
                            ) where

import Crawler.Class     (Crawler (..))
import Frontier.Class    (Frontier (..), NextUrl (..), UrlResult (..))
import Multithread.Class (Multithread (..))
import Restful.Class     (Restful (..))
import Restful.IO        (fetchGetImpl)
import Restful.Types     (Response, Url)
import Scrape            (scrapeUrls)
import Time.Class        (Millis (..), Time (..))

import           Control.Concurrent            (threadDelay)
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM        (atomically)
import           Control.Exception.Safe        (MonadCatch, MonadThrow)
import           Control.Monad                 (unless)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Hashable                 (Hashable (hash))
import           Data.Text                     (Text)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Data.Vector                   ((!), Vector)
import qualified Data.Vector as V
import           Network.HTTP.Client           (Manager, defaultManagerSettings, newManager)
import           StmContainers.Set             (Set)
import qualified StmContainers.Set as S

data Env f =
    Env { getHttp       :: !Manager
        , getNumThreads :: !Int
        , getCrawlers   :: !(Vector f)
        , getSleeping   :: !(Set Int)
        }

newtype MultiCrawler f a =
    MultiCrawler { unMultiCrawler :: ReaderT (Env f) IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance Frontier f => Crawler (MultiCrawler f) where

    addUrl :: Url -> MultiCrawler f ()
    addUrl url = do
        env <- MultiCrawler ask
        -- Perhaps it could hash once on thread parallelism, then one on host,
        -- and each number could be configured independently
        let p = hash url `mod` (getNumThreads env) -- This currently hashes the whole URL, not just the host.  Make this an option?
        -- Do the following (wake/insert) need to be together in raw STM?
        wake p
        liftIO $ insert (getCrawlers env ! p) url

    start :: MultiCrawler f ()
    start = do

        env <- MultiCrawler ask

        forConcurrently_ (V.zip [0..] (getCrawlers env))
                         (go (getHttp env))

        -- TODO return result or stats here

class Sleeper m where

    -- wastful n*n ?
    allSleeping :: m Bool

    rest :: Int -> m ()

    wake :: Int -> m ()

instance Sleeper (MultiCrawler f) where

    allSleeping :: MultiCrawler f Bool
    allSleeping = do
        env <- MultiCrawler ask
        liftIO . atomically $ do
            s <- S.size $ getSleeping env
            pure $ getNumThreads env == s

    rest :: Int -> MultiCrawler f ()
    rest p = do
        env <- MultiCrawler ask
        liftIO . atomically $ S.insert p (getSleeping env)

    wake :: Int -> MultiCrawler f ()
    wake p = do
        env <- MultiCrawler ask
        liftIO . atomically $ S.delete p (getSleeping env)

instance Restful (MultiCrawler f) where

    -- TODO: motivate the catch/throw dependencies here
    fetchGet :: Url -> MultiCrawler f (Either [Text] Response)
    fetchGet url = do        
        env <- MultiCrawler ask
        fetchGetImpl (getHttp env) url

instance Time (MultiCrawler f) where

    currentMillis =
        Millis . round . (* 1000) <$> liftIO getPOSIXTime

    wait (Millis ms) =
        liftIO . threadDelay $ 1000 * ms

runCrawler :: Frontier f => Int -> MultiCrawler f a -> IO a
runCrawler numThreads crawler = do
    http     <- newManager defaultManagerSettings
    ps       <- V.replicateM numThreads newFrontier
    sleeping <- S.newIO
    runReaderT (unMultiCrawler crawler) (Env http numThreads ps sleeping)

-- Maybe it makes no sense to group these, and demand everyone implements everything
instance Multithread (MultiCrawler f) where

    mapConcurrently :: Traversable t => (a -> MultiCrawler f b) -> t a -> MultiCrawler f (t b)
    mapConcurrently f xs =
        MultiCrawler $ do
            env <- ask
            liftIO $ A.mapConcurrently (unlift env . f) xs

    forConcurrently_ :: Traversable t => t a -> (a -> MultiCrawler f ()) -> MultiCrawler f ()
    forConcurrently_ xs f =
        MultiCrawler $ do
            env <- ask
            liftIO $ A.forConcurrently_ xs (unlift env . f)

unlift :: Env f -> MultiCrawler f b -> IO b
unlift env (MultiCrawler run) = runReaderT run env

go :: Frontier f => Manager -> (Int, f) -> MultiCrawler f ()
go http (i, p) = do

    as <- allSleeping

    unless as $ do

      now <- currentMillis

      liftIO (nextUrl p now) >>= \case

        NoMoreUrls -> do
            rest i -- right place for this? or end of method?
            liftIO $ putStrLn "Waiting"
            wait $ Millis 250
            go http (i, p)

        RetryIn ms -> do
            liftIO $ putStrLn ("Waiting " ++ show ms)
            wait ms
            go http (i, p)

        NextUrl url ->

            fetchGetImpl http url >>= \case

                Left _ ->
                    liftIO $ completed p url (UrlFailure (-1)) -- TODO code this

                Right response -> do

                    -- completed could probably not need liftIO 
                    liftIO $ completed p url Success -- TODO check

                    let urls = scrapeUrls response
                    mapM_ addUrl urls
                    go http (i, p)
