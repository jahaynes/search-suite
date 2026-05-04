{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs #-}

module Frontier.MultiplexedFrontier where

{-
    Pause this.  This isn't the right place to multiplex
-}

{-
import           Frontier.Class
import qualified Frontier.PoliteStmFrontier as P -- Pass this in instead!
import           Restful.Types

import           Control.Concurrent.STM
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Hashable
import qualified Data.Heap as H
import           Data.Heap                     (Heap)
import           Data.Time.Clock.POSIX
import           Data.Vector                   ((!), Vector)
import qualified Data.Vector as V

data Env = Env { getNumThreads :: !Int
               , getCrawlers   :: !(Vector P.PoliteStmFrontier)
               , crawlerHeap   :: !(TVar (Heap Int))
               }

newtype MultiplexedFrontier a =
    MultiplexedFrontier { unMultiplexedFrontier :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance Frontier MultiplexedFrontier where

    completed :: Url -> UrlResult -> MultiplexedFrontier ()
    completed url result = do
        Env nt cs _ <- MultiplexedFrontier ask
        let h = hash url `mod` nt
        liftIO $ P.signalUrlCompleted (cs ! h) url result

    insert :: Url -> MultiplexedFrontier ()
    insert url = do
        Env nt cs _ <- MultiplexedFrontier ask
        let h = hash url `mod` nt
        liftIO $ P.insertUrl (cs ! h) url

    nextUrl :: MultiplexedFrontier NextUrl
    nextUrl = do
        
        Env nt cs tch <- MultiplexedFrontier ask

        liftIO $ do

            -- Choose crawler
            atomically $ do
                ch <- readTVar tch
                undefined

            -- Route request

            now <- currentMillis
            undefined -- popEarliestHostUrl

currentMillis :: IO Millis
currentMillis = Millis . round . (* 1000) <$> getPOSIXTime

runMultiplexedFrontier :: Int -> MultiplexedFrontier a -> IO a
runMultiplexedFrontier numThreads multiplexedFrontier = do
    pCrawlers <- V.replicateM numThreads P.new
    crawlerHeap <- newTVarIO $ H.fromList [0..numThreads-1]
    runReaderT (unMultiplexedFrontier multiplexedFrontier) (Env numThreads pCrawlers crawlerHeap)
-}