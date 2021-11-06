{-# LANGUAGE LambdaCase #-}

module Main where

import           Errors.Errors
import           Job.YamlJobs
import qualified Network.Fetcher           as F

import           Page.Page
import           Page.Scrape
import qualified Pipeline.AllowedUrls      as A
import qualified Pipeline.Processor        as P
import qualified Pipeline.TimedFrontier    as TF
import qualified Pipeline.SqlTimedFrontier as STF
import qualified Reporter.Reporter         as R
import qualified Client.SearchApiClient    as C
import qualified Storage.Store             as S
import qualified Storage.WarcFileWriter    as W

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad.Extra
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.ByteString.Char8      (ByteString)
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)

getJobsYaml :: IO FilePath
getJobsYaml =
    let msg = "Must specify path to one jobs.yaml file"
    in
    getArgs >>= \case
        [fp] -> do let fileType = ".yaml"
                       goodFileType = take (length fileType) (reverse fp) == reverse fileType
                   exists <- doesFileExist fp
                   if exists && goodFileType
                       then pure fp
                       else error msg
        _    -> error msg

main :: IO ()
main = do

    Jobs jobs <- getJobsYaml >>= readJobs

    forConcurrently_ jobs $ \job -> do

        initialise job

        frontier <- STF.create ":memory:" 0.4
        --frontier <- STF.create "./foo.db" 0.4
        -- frontier <- TF.create 0.4

        Right processor <- runExceptT $ do allowedUrls    <- A.create
                                           fetcher        <- liftIO F.createFetcher
                                           client         <- liftIO C.create
                                           warcFileWriter <- liftIO W.create
                                           P.create frontier allowedUrls fetcher client warcFileWriter job

        reporter <- R.create
        md5Store <- S.create

        -- TODO investigate this
        replicateConcurrently_ 8 (run reporter md5Store processor)

        -- run reporter md5Store processor

run :: R.Reporter IO
    -> S.Store ByteString (ExceptT Error IO)
    -> P.Processor (ExceptT Error IO)
    -> IO ()
run reporter md5store processor = do

    (runExceptT $ P.p_step processor) >>= \case

        Left e -> do R.report reporter e
                     run reporter md5store processor

        Right Nothing -> do putStrLn "Thread sleeping"
                            threadDelay 1000000
                            run reporter md5store processor

        Right (Just page) -> do

            eSeen <- runExceptT $ S.s_member md5store (p_md5 page)

            case eSeen of
                Left e -> do R.report reporter e
                             run reporter md5store processor
                Right seen -> do
                    unless seen $ do
                        sr <- runExceptT $ do
                            S.s_put md5store (p_md5 page)
                            let urls = scrape page
                            P.p_submit processor (Just $ p_url page) urls
                        case sr of
                            Left e -> do R.report reporter e
                                         run reporter md5store processor
                            Right () -> pure ()
                    run reporter md5store processor
