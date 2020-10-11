{-# LANGUAGE LambdaCase #-}

module Pipeline.Processor ( Processor (..)
                          , create
                          ) where

import Client.SearchApiClient (SearchApiClient (..))
import Job.YamlJobs           (Action (..), Job (..))
import Network.Fetcher        (Fetcher (fetch))
import Page.Page              (Page (..))
import Pipeline.FrontierTypes (Now (..), Result (..))
import Pipeline.TimedFrontier (TimedFrontier (..))
import Storage.Store          (Store (..))
import Storage.WarcFileWriter (WarcFileWriter (..))
import Url

import Control.Concurrent
import Control.Monad          (filterM, forM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe             (fromJust)
import Data.Time.Clock

data Processor m =
    Processor { p_submit :: !([Url] -> m ())
              , p_step   :: !(m (Maybe Page))
              }

create :: MonadIO m
       => TimedFrontier m
       -> Store Host m
       -> Fetcher m
       -> SearchApiClient m
       -> WarcFileWriter
       -> Job
       -> m (Processor m)
create frontier allowedHosts fetcher searchApiClient warcFileWriter job = do

    let processor = 
            Processor { p_submit = submitImpl frontier allowedHosts
                      , p_step   = step frontier fetcher searchApiClient warcFileWriter (j_actions job)
                      }
    let urls = map (fromJust . mkUrl) (j_seedUrls job)
    seed frontier allowedHosts urls
    pure processor

seed :: MonadIO m => TimedFrontier m -> Store Host m -> [Url] -> m ()
seed frontier allowedHosts urls = do
    forM_ urls $ \url -> s_put allowedHosts (getHost url)
    submitImpl frontier allowedHosts urls

submitImpl :: MonadIO m => TimedFrontier m -> Store Host m -> [Url] -> m ()
submitImpl frontier allowedHosts urls = do
    allowed <- filterM (s_member allowedHosts . getHost) urls
    unless (null allowed) $ do
        now <- Now <$> liftIO getCurrentTime
        tf_submit frontier now allowed

step :: MonadIO m
     => TimedFrontier m
     -> Fetcher m
     -> SearchApiClient m
     -> WarcFileWriter
     -> [Action]
     -> m (Maybe Page)
step frontier fetcher searchApiClient warcFileWriter actions =

    let loop = do 

            now <- Now <$> liftIO getCurrentTime

            tf_nextUrl frontier now >>= \case

                Done -> pure Nothing

                WaitMicros micros -> do 
                    liftIO $ do putStrLn $ unwords ["Waiting", show micros, "micros"]
                                threadDelay micros
                    loop

                Url url -> do
                    page <- fetch fetcher url
                    mapM_ (act page) actions
                    pure (Just page)
    in loop

    where
    act page (Action "postTo" strUrl) = 
        let Just url = mkUrl strUrl
        in postToSearchApi searchApiClient url page

    act page (Action "writeWarc" warcFile) = do
        liftIO $ submit warcFileWriter warcFile page