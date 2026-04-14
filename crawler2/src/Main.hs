{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import Crawler
import Restful.Class (Restful (..))
import Restful.Types
import Scrape

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do

    let Just url = mkUrl "http://[::1]:9090/content/wikipedia_en_all_maxi/Johannes_Gutenberg"

    runCrawler (simple3 url)

simple3 :: (Applicative m, MonadIO m, Restful m) => Url -> m ()
simple3 url = do

    fetchGet url >>= \case

        Left l -> error $ show l

        Right response -> do
            liftIO . C8.putStrLn . getBody $ response
            liftIO $ print (getCode response)
            let urls = scrapeUrls response
            mapM_ (liftIO . print) urls
