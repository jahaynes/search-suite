{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import Crawler        (runCrawler)
import Frontier.Class (Frontier (..), Millis (..), NextUrl (..))
import Restful.Class  (Restful (..))
import Restful.Types  (mkUrl)
import Scrape         (scrapeUrls)

import Control.Concurrent     (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))

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

                Left l ->
                    error $ show l

                Right response -> do

                    completed url

                    let urls = scrapeUrls response
                    mapM_ insert urls
                    go

lPutStrLn :: MonadIO m => String -> m ()
lPutStrLn = liftIO . putStrLn