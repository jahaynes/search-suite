{-# LANGUAGE QuasiQuotes #-}

module Logger where

import           Control.Monad           (forM_)
import           Data.ByteString         (ByteString)
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text.IO as T

data LoggerType = CompactorLogger
                | ControllerLogger
                | ImporterLogger
                | IndexerLogger
                | QueryProcessorLogger
                | RegistryLogger
                | SpellingProcessorLogger
                | StructuredProcessorLogger
                | WarcFileReaderLogger
                | WarcIndexerLogger
                | TestLogger
                    deriving Show

data Logger =
    Logger { info   :: !([Text] -> IO ())
           , infoBs :: !([ByteString] -> IO ()) } -- TODO - eventually remove bs

createLogger :: LoggerType -> Logger
createLogger loggerType =
    Logger { info   = infoImpl loggerType
           , infoBs = infoBsImpl loggerType
           }

infoImpl :: LoggerType -> [Text] -> IO ()
infoImpl loggerType msgs = forM_ msgs $ \msg ->
    T.putStrLn [i|#{loggerType}: #{msg}|]

infoBsImpl :: LoggerType -> [ByteString] -> IO ()
infoBsImpl loggerType msgs = forM_ msgs $ \msg ->
    T.putStrLn [i|#{loggerType}: #{msg}|]
