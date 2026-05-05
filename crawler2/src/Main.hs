module Main ( main ) where

import Restful.Types              (Url, mkUrl)              
import Crawler.Class              (Crawler (..))
import Crawler.MultiCrawler       (MultiCrawler, runCrawler)
import Frontier.PoliteStmFrontier (PoliteStmFrontier)

numThreads :: Int
numThreads = 4

main :: IO ()
main =
    case mkUrl "http://127.0.0.1:3000" of  
        Nothing  -> error "Bad url"
        Just url -> runCrawler numThreads (job [url])

job :: Traversable t => t Url -> MultiCrawler PoliteStmFrontier ()
job seeds = do
    mapM_ addUrl seeds
    start