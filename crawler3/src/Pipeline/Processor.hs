{-# LANGUAGE LambdaCase #-}

module Pipeline.Processor ( Processor (..)
                          , create
                          ) where

import Client.SearchApiClient (SearchApiClient (..))
import Job.YamlJobs           (Action (..), Job (..))
import Network.Fetcher        (Fetcher (fetch))
import Page.Page              (Page (..))
import Pipeline.AllowedUrls   (AllowedUrls (..))
import Pipeline.FrontierTypes (Now (..), Result (..))
import Pipeline.TimedFrontier (TimedFrontier (..))
import Storage.WarcFileWriter (WarcFileWriter (..))
import Url

import Control.Concurrent
import Control.Monad          (filterM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe             (fromJust)
import Data.Time.Clock

data Processor m =
    Processor { p_submit :: !([Url] -> m ())
              , p_step   :: !(m (Maybe Page))
              }

create :: MonadIO m
       => TimedFrontier m
       -> AllowedUrls m
       -> Fetcher m
       -> SearchApiClient m
       -> WarcFileWriter
       -> Job
       -> m (Processor m)
create frontier allowedUrls fetcher searchApiClient warcFileWriter job = do

    let processor = 
            Processor { p_submit = submitImpl frontier allowedUrls
                      , p_step   = step frontier fetcher searchApiClient warcFileWriter (j_actions job)
                      }
    let urls = map (fromJust . mkUrl) (j_seedUrls job)
    seed frontier allowedUrls urls
    pure processor

seed :: MonadIO m => TimedFrontier m -> AllowedUrls m -> [Url] -> m ()
seed frontier allowedUrls urls = do
    mapM_ (allowUrlAndVariants allowedUrls) urls
    submitImpl frontier allowedUrls urls

submitImpl :: MonadIO m => TimedFrontier m -> AllowedUrls m -> [Url] -> m ()
submitImpl frontier allowedUrls urls = do

    allowed <- filterM (urlAllowed allowedUrls) urls

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
step frontier fetcher searchApiClient warcFileWriter actions = do

    myid <- liftIO myThreadId

    let loop = do 

            now@(Now n) <- Now <$> liftIO getCurrentTime
            nextUrl     <- tf_nextUrl frontier now
            later       <- liftIO getCurrentTime
            liftIO $ do
                let t = diffUTCTime later n
                when (t > 0.5) $ print (myid, "got url in", t)

            case nextUrl of

                Done -> pure Nothing

                WaitMicros micros -> do 
                    liftIO $ do putStrLn $ unwords ["Waiting", show micros, "micros"]
                                threadDelay micros
                    loop

                Url url -> do
                    page <- fetch fetcher url
                    mapM_ (act page) actions
                    pure (Just page)
    loop

    where
    act page (Action "postTo" strUrl) = 
        let Just url = mkUrl strUrl
        in postToSearchApi searchApiClient url page

    act page (Action "writeWarc" warcFile) = do
        liftIO $ submit warcFileWriter warcFile page
