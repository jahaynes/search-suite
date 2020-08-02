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
       -> Job
       -> m (Processor m)
create frontier allowedHosts fetcher searchApiClient job = do

    let processor = 
            Processor { p_submit = submit frontier allowedHosts
                      , p_step   = step frontier fetcher searchApiClient (j_actions job)
                      }
    let urls = map (fromJust . mkUrl) (j_seedUrls job)
    seed frontier allowedHosts urls
    pure processor

seed :: MonadIO m => TimedFrontier m -> Store Host m -> [Url] -> m ()
seed frontier allowedHosts urls = do
    forM_ urls $ \url -> s_put allowedHosts (getHost url)
    submit frontier allowedHosts urls

submit :: MonadIO m => TimedFrontier m -> Store Host m -> [Url] -> m ()
submit frontier allowedHosts urls = do
    allowed <- filterM (s_member allowedHosts . getHost) urls
    unless (null allowed) $ do
        now <- Now <$> liftIO getCurrentTime
        tf_submit frontier now allowed

step :: MonadIO m
     => TimedFrontier m
     -> Fetcher m
     -> SearchApiClient m
     -> [Action]
     -> m (Maybe Page)
step frontier fetcher searchApiClient actions =

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
