{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import Crawler        (runCrawler)
import Frontier.Class (Frontier (..), NextUrl (..))
import Restful.Class  (Restful (..))
import Restful.Types  (mkUrl)
import Scrape         (scrapeUrls)

import Control.Monad.IO.Class

main :: IO ()
main = runCrawler simple3

simple3 :: (Frontier m, MonadIO m, Restful m) => m ()
simple3 =
    case mkUrl "http://127.0.0.1:3000" of
        Nothing -> error "Bad url"
        Just url -> do
            insert url
            go

go :: (Frontier m, MonadIO m, Restful m) => m ()
go =

    nextUrl >>= \case

        NoMoreUrls ->
            liftIO $ putStrLn "No more urls.  Done"

        RetryInMicros _ -> error "RetryInMicros"

        NextUrl url ->

            fetchGet url >>= \case

                Left l ->
                    error $ show l

                Right response -> do
                    let urls = scrapeUrls response
                    mapM_ insert urls
                    go
