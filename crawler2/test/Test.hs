{-# LANGUAGE GeneralizedNewtypeDeriving,
             InstanceSigs #-}

module Main (main) where

import Restful.Class (Restful (..))
import Restful.Types (Response, Url)

import Control.Monad.Trans.Reader (ReaderT)
import Data.Text                  (Text)

data TestEnv = TestEnv

newtype TestCrawler m a =
    TestCrawler { unTestCrawler :: ReaderT TestEnv m a }
        deriving (Functor, Applicative, Monad, MonadFail)

main :: IO ()
main = putStrLn "Test suite not yet implemented."

instance Restful (TestCrawler m) where

    fetchGet :: Url -> TestCrawler m (Either [Text] Response)
    fetchGet url =
        undefined
