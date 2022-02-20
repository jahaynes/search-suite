{-# LANGUAGE OverloadedStrings #-}

module Pipeline.SqlTimedFrontier (create) where

import Metrics
import Pipeline.FrontierTypes
import Url

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad                    (forM_, unless, when)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Maybe                       (fromJust)
import qualified Data.Set as S
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.UUID                        (UUID, fromText, toText)
import qualified Data.UUID.V4                as U
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

newtype UrlRow =
    UrlRow Url

instance ToField UrlRow where
    toField (UrlRow url) =
        toField (show url)

instance FromField UrlRow where
    fromField x = do 
        s <- fromField x
        case mkUrl s of
            Nothing -> fail "Could not parse URL from DB"
            Just u  -> pure (UrlRow u)

data UrlHostRow = UrlHostRow !Url !Host

instance ToRow UrlHostRow where
  toRow (UrlHostRow url (Host host)) =
      toRow (show url, host)

data UrlTimeRow =
    UrlTimeRow !Url !UTCTime

instance FromRow UrlTimeRow where
    fromRow = UrlTimeRow <$> (fromJust . mkUrl <$> field)
                         <*> (fromEpochMicros  <$> field)

data UpsertHostTime =
    UpsertHostTime !Host !UTCTime

instance ToRow UpsertHostTime where
  toRow (UpsertHostTime (Host host) time) =
      toRow (host, microsSinceEpoch time)

data InsertHostTime =
    InsertHostTime !Host !UTCTime

instance ToRow InsertHostTime where
  toRow (InsertHostTime (Host host) time) =
      toRow (host, microsSinceEpoch time)

newtype SqlUUID =
    SqlUUID UUID

instance ToField SqlUUID where
    toField (SqlUUID uuid) = toField . toText $ uuid

instance FromField SqlUUID where
    fromField x = do 
        s <- fromField x
        case fromText s of
            Nothing -> fail "Could not parse UUID from DB"
            Just u  -> pure (SqlUUID u)

newtype Count =
    Count Int

instance FromRow Count where
    fromRow = Count <$> field

newtype ConnectionLock = 
    ConnectionLock (TVar (Either ThreadId Connection))

newConnectionLock :: FilePath -> IO ConnectionLock
newConnectionLock fp = do
    c <- open fp
    createSqlTables c
    ConnectionLock <$> newTVarIO (Right c)

-- rewrite as with..
acquire :: ConnectionLock -> IO Connection
acquire (ConnectionLock tv) = do
    threadId <- myThreadId
    atomically $ do
        v <- readTVar tv
        case v of
            Left _ -> retry
            Right conn -> do
                writeTVar tv (Left threadId)
                pure conn

release :: ConnectionLock -> Connection -> IO ()
release (ConnectionLock tv) conn = do 
    threadId <- myThreadId
    atomically $ do
        v <- readTVar tv
        case v of
            Left ti | ti == threadId -> writeTVar tv (Right conn)
            _ -> error "Bad release!"

-- TODO test
microsSinceEpoch :: UTCTime -> Int
microsSinceEpoch = floor . (1e6 *) . utcTimeToPOSIXSeconds

fromEpochMicros :: Int -> UTCTime
fromEpochMicros = posixSecondsToUTCTime . (1e-6 *) . fromIntegral

create :: MonadIO m => FilePath -> NominalDiffTime -> IO (TimedFrontier m)
create fp perHostDelay = do

    -- TODO return a connection lock factory that lends out based on threadid ?
    lock <- newConnectionLock fp
           
    pure $ TimedFrontier { tf_submit  = submitUrlImpl lock
                         , tf_nextUrl = nextUrl perHostDelay lock
                         } 

nextUrl :: MonadIO m => NominalDiffTime -> ConnectionLock -> Now -> m Result
nextUrl perHostDelay lock now@(Now n) = liftIO $ timeMetric "nextUrl" $ do

    uuid <- SqlUUID <$> U.nextRandom

    c <- acquire lock

    r <- withTransaction c $ do

        --liftIO $ print ("Marking", uuid)
        execute c sqlMarkUrl (Only uuid)

        mUrlTime <- handle <$> query c sqlSelectUrl (Only uuid)
        --liftIO $ print ("found", mUrlTime)

        case mUrlTime of
            
            Nothing -> pure Done

            Just (UrlTimeRow url time) -> do

                execute c sqlDeleteUrl (Only uuid)
                case getMicrosToWait now time of
                    Just micros -> pure $ WaitMicros micros
                    Nothing     -> do permitAccessAt c (getHost url) PushBackTime $! addUTCTime perHostDelay n 
                                      pure $ Url url

    release lock c

    pure r

    where
    handle  [] = Nothing
    handle [x] = Just x
    handle   _ = error "Too many urls"

    sqlMarkUrl :: Query
    sqlMarkUrl =
        " UPDATE host_urls         \
        \ SET takenBy = ?          \ 
        \ WHERE url IN (           \
        \   SELECT url             \
        \   FROM host_urls U       \
        \   JOIN hostnames_times T \
        \   ON U.host = T.host     \
        \   ORDER BY time          \
        \   LIMIT 1                \
        \ )                        \
        \ AND takenBy IS NULL      "

    sqlSelectUrl :: Query
    sqlSelectUrl =
        " SELECT url, time       \
        \ FROM host_urls H       \
        \ JOIN hostnames_times T \
        \ ON H.host = T.host     \
        \ WHERE takenBy = ?      "

    sqlDeleteUrl :: Query
    sqlDeleteUrl =
        " DELETE            \
        \ FROM host_urls    \
        \ WHERE takenBy = ? "

permitAccessAt :: MonadIO m => Connection -> Host -> PushBackTime -> UTCTime -> m ()
permitAccessAt c host pushBackTime nextTime =  
    liftIO $
        case pushBackTime of
            PushBackTime     -> execute c sqlUpsertHostTime (UpsertHostTime host nextTime)
            DontPushBackTime -> execute c sqlInsertHostTime (InsertHostTime host nextTime)

    where
    sqlUpsertHostTime :: Query
    sqlUpsertHostTime =
        " INSERT INTO hostnames_times \
        \ (host, time) VALUES         \
        \ (   ?,    ?)                \
        \ ON CONFLICT (host)          \
        \ DO UPDATE                   \
        \ SET time = excluded.time    \
        \ WHERE excluded.time > time  "

    sqlInsertHostTime :: Query
    sqlInsertHostTime =
        " INSERT OR IGNORE INTO hostnames_times \
        \ (host, time) VALUES                   \
        \ (   ?,    ?)                          "

submitUrlImpl :: MonadIO m => ConnectionLock -> Now -> Maybe Url -> [Url] -> m ()
submitUrlImpl lock (Now now) fromUrl urls = liftIO $ timeMetric "submitUrl" $ do

    let distinctUrls = S.toList $ S.fromList urls

    unless (null distinctUrls) $ do

        c <- acquire lock

        withTransaction c $

            forM_ distinctUrls $ \url -> do

                execute c sqlSubmitUrl (UrlRow url, UrlRow <$> fromUrl)
                changed <- (==1) <$> changes c
                when changed $ do
                    permitAccessAt c (getHost url) DontPushBackTime now
                    execute c sqlInsertHostUrl (UrlHostRow url (getHost url))

        release lock c

    where
    sqlSubmitUrl :: Query
    sqlSubmitUrl =
        " INSERT OR IGNORE INTO completed_urls \
        \ (url, fromUrl) VALUES                \
        \ (  ?,       ?)                       "

    sqlInsertHostUrl :: Query
    sqlInsertHostUrl =
        " INSERT INTO host_urls \
        \ (url, host) VALUES    \
        \ (  ?,    ?)           "

createSqlTables :: Connection -> IO ()
createSqlTables c = do
    execute_ c schemaCompleted
    mapM_ (execute_ c) schemaHostUrls
    mapM_ (execute_ c) schemaHostnamesTimes

    where

    schemaCompleted :: Query
    schemaCompleted =
        " CREATE TABLE IF NOT EXISTS              \
        \   completed_urls                        \
        \     ( url     TEXT PRIMARY KEY NOT NULL \
        \     , fromUrl TEXT                      \
        \     )                                   "

    schemaHostUrls :: [Query]
    schemaHostUrls =

        [   " CREATE TABLE IF NOT EXISTS              \
            \   host_urls                             \
            \     ( url     TEXT PRIMARY KEY NOT NULL \
            \     , host    TEXT             NOT NULL \
            \     , takenBy TEXT                      \
            \     )                                   "

        ,   " CREATE INDEX IF NOT EXISTS idx_hosts \
            \ ON host_urls (host);                 "

        ,   " CREATE INDEX IF NOT EXISTS idx_takenBy \
            \ ON host_urls (takenBy);                "

        ]

    schemaHostnamesTimes :: [Query]
    schemaHostnamesTimes =

        [   " CREATE TABLE IF NOT EXISTS              \
            \   hostnames_times                       \
            \     ( host TEXT    PRIMARY KEY NOT NULL \
            \     , time INTEGER             NOT NULL \
            \     )                                   "

        ,   " CREATE INDEX IF NOT EXISTS idx_time \
            \ ON hostnames_times (time);          "

        ]
