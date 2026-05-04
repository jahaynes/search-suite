{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import qualified Frontier.PoliteStmFrontier as P



import Crawler        (runCrawler)
import Frontier.Class (Frontier (..), Millis (..), NextUrl (..), UrlResult (..))
import Restful.Class  (Restful (..))
import Restful.Types  (Url, mkUrl)
import Scrape         (scrapeUrls)

import Restful.IO (fetchGetImpl)
import Control.Concurrent     --  (threadDelay)
import Control.Concurrent.Async -- (forConcurrently_, replicateConcurrently)
import Control.Monad
import Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client           (Manager, defaultManagerSettings, newManager)

import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Data.Hashable

numThreads :: Int
numThreads = 4

main :: IO ()
main = do

    ps <- V.replicateM numThreads P.new

    case mkUrl "http://127.0.0.1:3000" of
        
        Nothing -> error "Bad url"
        
        Just url -> do

            addUrl ps url

            js <- forM ps $ \p -> async $ do

                http <- newManager defaultManagerSettings
                
                go http p ps

            mapM_ wait js

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


currentMillis :: IO Millis
currentMillis = Millis . round . (* 1000) <$> getPOSIXTime

addUrl :: Vector P.PoliteStmFrontier -> Url -> IO ()
addUrl ps url =
    let h = hash url `mod` numThreads
    in P.insertUrl (ps ! h) url



{-
main :: IO ()
main = runCrawler simple3

simple3 :: (Frontier m, MonadIO m, Restful m) => m ()
simple3 =
    case mkUrl "http://127.0.0.1:3000" of
        Nothing -> error "Bad url"
        Just url -> do
            liftIO $ putStrLn $ "Inserting url: " ++ show url
            insert url
            go

go :: (Frontier m, MonadIO m, Restful m) => m ()
go =

    nextUrl >>= \case

        NoMoreUrls ->
            lPutStrLn "No more urls.  Done"

        RetryIn (Millis ms) -> do
            lPutStrLn ("Waiting " ++ show ms ++ "ms")
            liftIO (threadDelay (1000 * ms))
            go

        NextUrl url ->

            fetchGet url >>= \case

                Left _ ->
                    completed url (UrlFailure (-1)) -- TODO code this

                Right response -> do

                    completed url Success -- TODO check

                    let urls = scrapeUrls response
                    mapM_ insert urls
                    go

lPutStrLn :: MonadIO m => String -> m ()
lPutStrLn = liftIO . putStrLn
-}