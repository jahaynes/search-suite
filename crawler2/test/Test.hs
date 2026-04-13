{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad.Trans.Reader (ReaderT)

data TestEnv = TestEnv

newtype TestCrawler m a =
    TestCrawler { unTestCrawler :: ReaderT TestEnv m a }
        deriving (Functor, Applicative, Monad, MonadFail)

main :: IO ()
main = putStrLn "Test suite not yet implemented."
