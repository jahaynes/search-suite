{-# LANGUAGE LambdaCase #-}

module Main where

import           Errors.Errors
import           Job.YamlJobs                   (Jobs (..), initialise, j_maxPages, readJobs)
import qualified Network.Fetcher           as F

import           Page.Page
import           Page.Scrape
import qualified Pipeline.AllowedUrls      as A
import qualified Pipeline.Processor        as P
-- import qualified Pipeline.TimedFrontier    as TF
import qualified Pipeline.SqlTimedFrontier as STF
import qualified Reporter.Reporter         as R
import qualified Client.SearchApiClient    as C
import qualified Storage.Store             as S
import qualified Storage.WarcFileWriter    as W

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async
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

    Jobs js <- getJobsYaml >>= readJobs

    forConcurrently_ js $ \job -> do

        initialise job

        frontier <- STF.create ":memory:" 0.4
        --frontier <- STF.create "./foo.db" 0.4
        -- frontier <- TF.create 0.4

        Right processor <- runExceptT $ do allowedUrls    <- A.create
                                           fetcher        <- liftIO $ F.createFetcher Nothing -- TODO accept proxy settings
                                           client         <- liftIO C.create
                                           warcFileWriter <- liftIO W.create
                                           P.create frontier allowedUrls fetcher client warcFileWriter job

        reporter <- R.create
        md5Store <- S.create


        run reporter md5Store processor (j_maxPages job)

run :: R.Reporter IO
    -> S.Store ByteString (ExceptT Error IO)
    -> P.Processor (ExceptT Error IO)
    -> Maybe Int
    -> IO ()
run reporter md5store processor maxPages = do

    loop 0

    where
    loop :: Int -> IO ()
    loop n =

        case maxPages of

            Just mp | n >= mp ->

                putStrLn "Hit page limit"

            _ ->

                runExceptT (P.p_step processor) >>= \case

                    Left e -> do
                        R.report reporter e
                        loop n

                    Right Nothing -> do
                        putStrLn "Thread sleeping"
                        threadDelay 1000000
                        loop n

                    Right (Just page) ->

                        runExceptT (S.s_member md5store (p_md5 page)) >>= \case

                            Left e -> do
                                R.report reporter e
                                loop n

                            Right True ->
                                loop n

                            Right False -> do
                                sr <- runExceptT $ do
                                    S.s_put md5store (p_md5 page)
                                    let urls = scrape page
                                    P.p_submit processor (Just $ p_url page) urls

                                case sr of
                                        Left e -> do
                                            R.report reporter e
                                            loop n
                                        Right () ->
                                            loop (n+1)
