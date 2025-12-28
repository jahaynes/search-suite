-- TODO make use of this or remove
module TimingInfo () where

import           Data.IORef           (IORef, modifyIORef', newIORef, readIORef)
import           Data.List            (foldl')
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import           Data.Text            (Text)
import           Data.Time.Clock      (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

newtype MTimingInfo =
    MTimingInfo (IORef TimingInfo)

data TimingInfo =
    TimingInfo { startTime :: !UTCTime
               , current   :: !Text
               , jobs      :: !(Map Text NominalDiffTime)
               }

start :: Text -> IO MTimingInfo
start label = do
    now <- getCurrentTime
    ioref <- newIORef TimingInfo { startTime = now
                                 , current   = label
                                 , jobs      = mempty
                                 }
    pure $ MTimingInfo ioref

switch :: MTimingInfo -> Text -> IO ()
switch (MTimingInfo mti) label = do
    now <- getCurrentTime
    modifyIORef' mti $ \ti -> do
        let taken = diffUTCTime now (startTime ti)
        TimingInfo { startTime = now
                   , current   = label
                   , jobs      = M.insert (current ti) taken (jobs ti)
                   }

done :: MTimingInfo -> IO (Map Text NominalDiffTime)
done (MTimingInfo mti) = do
    ti <- readIORef mti
    now <- getCurrentTime
    let taken = diffUTCTime now (startTime ti)
    pure $ M.insert (current ti) taken (jobs ti)

total :: Map k NominalDiffTime -> NominalDiffTime
total = foldl' (+) 0 . M.elems