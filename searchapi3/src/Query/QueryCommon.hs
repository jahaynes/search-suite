{-# LANGUAGE OverloadedStrings #-}

module Query.QueryCommon ( queryComponent
                         , withLocks
                         ) where

import Component   ( Component )
import Environment ( Environment (..) )
import Logger      ( Logger (..) )
import Registry    ( Registry (..) )
import Types       ( CollectionName )

import           Control.Concurrent.STM      (atomically)
import           Control.DeepSeq             (NFData, deepseq)
import           Control.Exception.Safe      (catchAnyDeep)
import           Data.Aeson                  (FromJSON, eitherDecodeStrict')
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as S
import           GHC.IO.Exception            (ExitCode (..))
import           System.Process.ByteString   (readProcessWithExitCode)
import           UnliftIO.Exception          (bracket)

withLocks :: NFData a => Registry -> CollectionName -> ([Component] -> IO a) -> IO a
withLocks reg collectionName f =
    bracket acquire release $ \cmps -> do
        y <- f cmps -- TODO: should this be catchAnyDeep
        y `deepseq` pure y
    where
    acquire = atomically $ do
        components <- S.toList <$> viewCollectionComponents reg collectionName
        mapM_ (takeLock reg) components
        pure components
    release = mapM_ (releaseLockIO reg)

queryComponent :: (FromJSON a, NFData a) => Environment
                                         -> Logger
                                         -> [String]
                                         -> ByteString
                                         -> IO (Either String a)
queryComponent env logger execParams queryStr =

    let job = do logExecution

                 (exitcode, stdout, stderr) <- readProcessWithExitCode (indexerBinary env) execParams queryStr

                 case exitcode of

                     ExitSuccess -> do
                         C8.putStr stderr -- TODO don't
                         Right <$> decodeOutput stdout

                     _  -> do
                        infoBs logger ["stdout was: " <> stdout]
                        pure . Left $ C8.unpack stderr

        handle = pure . Left . show

    in catchAnyDeep job handle

    where
    decodeOutput :: FromJSON a => ByteString -> IO a
    decodeOutput stdout =
        case eitherDecodeStrict' stdout of
            Right r -> pure r
            Left _  -> error $ "Could not decode output: " <> take 100 (C8.unpack stdout) <> "..."

    logExecution :: IO ()
    logExecution = infoBs logger
                 . (\l -> [l])
                 . C8.pack
                 $ unwords [ "Running binary: "
                           , show (indexerBinary env)
                           , show execParams
                           ]
