{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import           Frontier.Class                  (Millis (..), NextUrl (..), UrlResult (..))
import           Frontier.PoliteStmFrontier      (PoliteStmFrontier)
import qualified Frontier.PoliteStmFrontier as P
import           Restful.Types                   (Url, mkUrl)
import           Restful.IO                      (fetchGetImpl)
import           Scrape                          (scrapeUrls)

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Monad            (forM)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           Data.Vector              ((!), Vector)
import qualified Data.Vector as V
import           Data.Hashable            (Hashable(hash))
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)

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

currentMillis :: IO Millis
currentMillis = Millis . round . (* 1000) <$> getPOSIXTime

addUrl :: Vector PoliteStmFrontier -> Url -> IO ()
addUrl ps url =
    let h = hash url `mod` numThreads
    in P.insertUrl (ps ! h) url
