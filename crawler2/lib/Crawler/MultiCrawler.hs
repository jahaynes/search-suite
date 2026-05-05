{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs,
             LambdaCase #-}

module Crawler.MultiCrawler where

import           Crawler.Class
import           Frontier.Class                  (NextUrl (..), UrlResult (..))
-- TODO: hide P behind class:
import           Frontier.PoliteStmFrontier      (PoliteStmFrontier)
import qualified Frontier.PoliteStmFrontier as P
import           Restful.Class
import           Restful.IO                      (fetchGetImpl)
import           Restful.Types                   (Response, Url, mkUrl)
import           Scrape                          (scrapeUrls)
import           Time.Class                      (Millis (..))

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (Async, async)
import           Control.Exception.Safe     (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Hashable              (Hashable (hash))
import           Data.Text                  (Text)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Data.Vector                ((!), Vector)
import qualified Data.Vector as V
import           Network.HTTP.Client        (Manager, defaultManagerSettings, newManager)

numThreads :: Int
numThreads = 4

data Env =
    Env !Manager !(Vector PoliteStmFrontier)

newtype MultiCrawler a =
    MultiCrawler { unMultiCrawler :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance Crawler MultiCrawler where

    addUrl :: Url -> MultiCrawler ()
    addUrl url = do
        Env _ ps <- MultiCrawler ask
        let h = hash url `mod` numThreads
        liftIO $ P.insertUrl (ps ! h) url

    start :: MultiCrawler (Async ())
    start = do
        
        _ <- fetchGet undefined
        
        liftIO . async $ pure ()

instance Restful MultiCrawler where

    -- TODO: motivate the catch/throw dependencies here
    fetchGet :: Url -> MultiCrawler (Either [Text] Response)
    fetchGet url = do        
        Env http _ <- MultiCrawler ask
        fetchGetImpl http url
        
create :: Int -> IO ()
create numThreads = do
    
    ps <- V.replicateM numThreads P.new

    
    pure ()



{-
go :: Manager -> PoliteStmFrontier -> Vector PoliteStmFrontier -> IO ()
go http p ps = do

    now <- currentMillis

    P.popEarliestHostUrl p now >>= \case

        NoMoreUrls -> do
            threadDelay (250000)
            putStrLn "No more urls.  Waited"
            go http p ps

        RetryIn (Millis ms) -> do
            putStrLn ("Waiting " ++ show ms ++ "ms")
            threadDelay (1000 * ms)
            go http p ps

        NextUrl url ->

            fetchGetImpl http url >>= \case

                Left _ ->
                    P.signalUrlCompleted p url (UrlFailure (-1)) -- TODO code this

                Right response -> do

                    P.signalUrlCompleted p url Success -- TODO check

                    let urls = scrapeUrls response
                    mapM_ (addUrl ps) urls
                    go http p ps
-}


-- Capability?
--currentMillis :: IO Millis
--currentMillis = Millis . round . (* 1000) <$> getPOSIXTime


{-

import Frontier.Class             (Frontier (..), Millis (..), NextUrl (..), UrlResult (..))
import Frontier.PoliteStmFrontier (PoliteStmFrontier, insertUrl, new, popEarliestHostUrl, signalUrlCompleted)
import Multithread.Class          (Multithread (..))
import Restful.Class              (Restful (..))
import Restful.IO                 (fetchGetImpl)
import Restful.Types              (Url, Response)

import qualified Control.Concurrent.Async as A
import           Control.Exception.Safe        (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Text                     (Text)
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client           (Manager, defaultManagerSettings, newManager)

data Env =
    Env !PoliteStmFrontier !Manager

newtype Crawler a =
    Crawler { unCrawler :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance Restful Crawler where

    fetchGet :: Url -> Crawler (Either [Text] Response)
    fetchGet url = do
        Env _ http <- Crawler ask
        fetchGetImpl http url

instance Multithread Crawler where

    mapConcurrently :: Traversable t => (a -> Crawler b) -> t a -> Crawler (t b)
    mapConcurrently f xs =
        Crawler $ do
            env <- ask
            liftIO $ A.mapConcurrently (go env) xs
        where
        go env x = unlift $ f x
            where
            unlift :: Crawler b -> IO b
            unlift (Crawler run) = runReaderT run env

instance Frontier Crawler where

    completed :: Url -> UrlResult -> Crawler ()
    completed url result = do
        Env frontier _ <- Crawler ask
        liftIO $ signalUrlCompleted frontier url result

    insert :: Url -> Crawler ()
    insert url = do
        Env frontier _ <- Crawler ask
        liftIO $ insertUrl frontier url

    nextUrl :: Crawler NextUrl
    nextUrl = do
        Env frontier _ <- Crawler ask
        liftIO $ do
            now <- currentMillis
            popEarliestHostUrl frontier now

currentMillis :: IO Millis
currentMillis = Millis . round . (* 1000) <$> getPOSIXTime

runCrawler :: Crawler a -> IO a
runCrawler crawler = do
    frontier <- new
    http <- newManager defaultManagerSettings
    runReaderT (unCrawler crawler) (Env frontier http)

-}