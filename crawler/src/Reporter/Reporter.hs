{-# LANGUAGE OverloadedStrings #-}

module Reporter.Reporter where

import Errors.Errors

import           Control.Concurrent
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

newtype Reporter m =
    Reporter { report :: Error -> m () }

create :: IO (Reporter IO)
create = pure $ Reporter reportImpl

reportImpl :: Error -> IO ()
reportImpl err = do 
    threadId <- show <$> myThreadId
    L8.appendFile ("errors_" <> threadId) $! encode err <> "\n"
    case err of
        PostError{} -> putStrLn "Warning: could not post!"
        _           -> pure ()