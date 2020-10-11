{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Storage.WarcFileWriter where

import Data.Warc.Body
import Data.Warc.Header
import Data.Warc.HeaderLine
import Data.Warc.Key
import Data.Warc.Value
import Data.Warc.WarcEntry
import Page.Page            (Page (..))

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBQueue      (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as Q
import           Control.Monad                       (filterM, replicateM_)
import qualified Data.ByteString.Char8 as C8
import           Data.UUID                           (toASCIIBytes)
import qualified Data.UUID.V4 as U
import           Debug.Trace                         (trace)
import           StmContainers.Map                   (Map)
import qualified StmContainers.Map as M
import           StmContainers.Set                   (Set)
import qualified StmContainers.Set as S

import           ListT

-- TODO assumes uncompressed input
data WarcFileWriter = 
    WarcFileWriter { submit            :: !(FilePath -> Page -> IO ())
                   }

newtype Lanes =
    Lanes (Map FilePath (TBQueue Page))

newtype Locks =
    Locks (Set FilePath)

create :: IO WarcFileWriter
create = do

    lanes <- Lanes <$> M.newIO

    locks <- Locks <$> S.newIO

    -- hold onto workers somehow?
    replicateM_ 4 (async $ runWorker locks lanes)

    pure $ WarcFileWriter { submit = submitImpl lanes
                          }

submitImpl :: Lanes -> FilePath -> Page -> IO ()
submitImpl (Lanes lanes) filePath page = do
    atomically $
        M.lookup filePath lanes >>= \case
            Just lane -> Q.writeTBQueue lane page
            Nothing -> do
                lane <- Q.newTBQueue 10
                Q.writeTBQueue lane page
                trace "Inserting" M.insert lane filePath lanes

-- todo resourcet around locking
runWorker :: Locks -> Lanes -> IO ()
runWorker (Locks locks) (Lanes lanes) = go
    where
    go = do
        (filePath, page) <- atomically $ do
            allLanes      <- toList (M.listT lanes)
            unlockedLanes <- filterM (\(fp,_) -> not <$> S.lookup fp locks) allLanes
            lanesWithWork <- filterM (\(_,la) -> not <$> Q.isEmptyTBQueue la) unlockedLanes
            case lanesWithWork of
                []           -> retry
                ((fp, la):_) -> do
                    S.insert fp locks
                    p <- Q.readTBQueue la
                    pure (fp, p)

        uuid <- toASCIIBytes <$> U.nextRandom

        let url = C8.pack . show $ p_url page

        let we = WarcEntry (WarcHeader (WarcVersion "1.0")
                                       [ HeaderLine (MandatoryKey WarcRecordId)  (StringValue uuid)
                                       , HeaderLine (MandatoryKey ContentLength) (IntValue . C8.length . p_body $ page)
                                       , HeaderLine (OptionalKey  WarcTargetURI) (StringValue url)
                                       ])
                           (UncompressedBody (p_body page))

        C8.appendFile filePath (toByteString we)

        -- unlock the queue
        atomically $ do
            S.lookup filePath locks >>= \case
                True  -> S.delete filePath locks
                False -> trace "Warning, releasing non-existant lock" $ pure ()
        go