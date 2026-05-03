{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs #-}

module Frontier.MultiplexedFrontier where

import Frontier.Class
import Frontier.PoliteStmFrontier -- Pass this in instead!
import Restful.Types

import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Hashable
import           Data.Vector ((!), Vector)
import qualified Data.Vector as V

data Env = Env { numThreads :: !Int }

newtype MultiplexedFrontier a =
    MultiplexedFrontier { unMultiplexedFrontier :: ReaderT Env IO a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance Frontier MultiplexedFrontier where

    completed :: Url -> UrlResult -> MultiplexedFrontier ()
    completed = undefined

    insert :: Url -> MultiplexedFrontier ()
    insert url = do
        
        nt <- numThreads <$> MultiplexedFrontier ask
        
        let h = hash url `mod` nt

        undefined

    nextUrl :: MultiplexedFrontier NextUrl
    nextUrl = undefined

runMultiplexedFrontier :: Int -> MultiplexedFrontier a -> IO a
runMultiplexedFrontier numThreads multiplexedFrontier = do

    pCrawlers <- V.replicateM numThreads new

    undefined