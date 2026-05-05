{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs,
             LambdaCase #-}

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
import           Control.Exception.Safe        (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Hashable                 (Hashable (hash))
import           Data.Text                     (Text)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Data.Vector                   ((!), Vector)
import qualified Data.Vector as V
import           Network.HTTP.Client           (Manager, defaultManagerSettings, newManager)

data Env f =
    Env !Manager !Int !(Vector f)

newtype MultiCrawler f a =
    MultiCrawler { unMultiCrawler :: ReaderT (Env f) IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance Frontier f => Crawler (MultiCrawler f) where

    addUrl :: Url -> MultiCrawler f ()
    addUrl url = do
        Env _ nt ps <- MultiCrawler ask
        let h = hash url `mod` nt           -- This currently hashes the whole URL, not just the host.  Make this an option?
        liftIO $ insert (ps ! h) url

    start :: MultiCrawler f ()
    start = do
        Env http _ ps <- MultiCrawler ask
        forConcurrently_ ps (go http)

instance Restful (MultiCrawler f) where

    -- TODO: motivate the catch/throw dependencies here
    fetchGet :: Url -> MultiCrawler f (Either [Text] Response)
    fetchGet url = do        
        Env http _ _ <- MultiCrawler ask
        fetchGetImpl http url

instance Time (MultiCrawler f) where

    currentMillis =
        Millis . round . (* 1000) <$> liftIO getPOSIXTime

    wait (Millis ms) =
        liftIO . threadDelay $ 1000 * ms

runCrawler :: Frontier f => Int -> MultiCrawler f a -> IO a
runCrawler numThreads crawler = do
    http <- newManager defaultManagerSettings
    ps <- V.replicateM numThreads newFrontier
    runReaderT (unMultiCrawler crawler) (Env http numThreads ps)

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

go :: Frontier f => Manager -> f -> MultiCrawler f ()
go http p = do

    now <- currentMillis

    liftIO (nextUrl p now) >>= \case

        NoMoreUrls -> do
            wait $ Millis 250
            liftIO $ putStrLn "No more urls.  Waited"
            go http p

        RetryIn ms -> do
            liftIO $ putStrLn ("Waiting " ++ show ms)
            wait ms
            go http p

        NextUrl url ->

            fetchGetImpl http url >>= \case

                Left _ ->
                    liftIO $ completed p url (UrlFailure (-1)) -- TODO code this

                Right response -> do

                    liftIO $ completed p url Success -- TODO check

                    let urls = scrapeUrls response
                    mapM_ addUrl urls
                    go http p
