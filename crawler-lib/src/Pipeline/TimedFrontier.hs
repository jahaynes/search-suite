{-# LANGUAGE LambdaCase #-}

module Pipeline.TimedFrontier ( TimedFrontier (..)
                              , create
                              ) where

import           Pipeline.FrontierTypes
import qualified Storage.MultiMap       as PM
import           Url

import           Control.Concurrent.STM
import           Control.Monad                 (when, unless)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Data.Map.Strict         as M
import           Data.Time.Clock
import           Debug.Trace
import qualified ListT                   as L
import qualified StmContainers.Multimap  as MM
import qualified StmContainers.Set       as S

create :: MonadIO m => NominalDiffTime -> IO (TimedFrontier m)
create perHostDelay = do

    completedUrls    <- S.newIO

    tvHostnamesTimes <- newTVarIO M.empty

    tvTimesHostnames <- PM.create

    hostUrls         <- MM.newIO

    let permitAccessAt :: Host -> PushBackTime -> UTCTime -> STM ()
        permitAccessAt host pushBackTime nextTime = do
            hnt <- readTVar tvHostnamesTimes
            case M.lookup host hnt of

                Nothing -> do
                    writeTVar tvHostnamesTimes $! M.insert host nextTime hnt
                    PM.mm_put tvTimesHostnames nextTime host

                Just time -> when (pushBackTime == PushBackTime && nextTime > time) $ do
                    writeTVar tvHostnamesTimes $! M.insert host nextTime hnt
                    PM.mm_delete tvTimesHostnames     time host
                    PM.mm_put    tvTimesHostnames nextTime host

    let submit :: Now -> Url -> STM ()
        submit (Now now) url = do
            completed <- S.lookup url completedUrls
            unless completed $ do
                permitAccessAt (getHost url) DontPushBackTime now
                S.insert url completedUrls
                MM.insert url (getHost url) hostUrls

    let submitM :: MonadIO m => Now -> Maybe Url -> [Url] -> m ()
        submitM now fromUrl urls = liftIO . atomically $ mapM_ (submit now) urls

    let unregisterAccess :: Host -> STM ()
        unregisterAccess host = do
            hnt <- readTVar tvHostnamesTimes
            case M.lookup host hnt of
                Nothing   -> pure ()
                Just time -> PM.mm_delete tvTimesHostnames time host
            writeTVar tvHostnamesTimes $! M.delete host hnt

    let nextUrl :: Now -> STM Result
        nextUrl now@(Now n) =

            PM.mm_lookupMin tvTimesHostnames >>= \case

                -- No per-host delays: take any host
                Nothing ->
                    L.head (MM.listT hostUrls) >>= \case
                        Nothing        -> trace "DONE DONE DONE" (pure Done)
                        Just (host, _) -> getUrls host

                -- Host can be done soonest.  No earler than time though.
                Just (time, host) ->
                    case getMicrosToWait now time of
                        Just micros -> pure $ WaitMicros micros
                        Nothing     -> getUrls host

            where
            getUrls host =
                MM.lookupByKey host hostUrls >>= \case
                    Nothing   -> error "cannot happen 1"
                    Just urls -> popUrl urls

            popUrl urls =
                L.head (S.listT urls) >>= \case
                    Nothing -> error "maybe cannot happen"
                    Just url -> do
                        let host = getHost url
                        numUrls <- S.size urls
                        if numUrls == 1
                            then do MM.deleteByKey host hostUrls
                                    unregisterAccess host
                            else do MM.delete url host hostUrls
                                    permitAccessAt host PushBackTime $! addUTCTime perHostDelay n 
                        pure (Url url)

    pure $ TimedFrontier
        { tf_submit    = submitM
        , tf_nextUrl   = liftIO . atomically . nextUrl
        }
