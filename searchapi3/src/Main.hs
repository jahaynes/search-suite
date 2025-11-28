{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compactor              (Compactor (..), createCompactor)
import Controllers.Controller (runController)
import Environment            (Environment (..), loadEnvironment)
import Importer               (Importer (importCollection), createImporter)
import Indexer                (createIndexer)
import Network.Fetcher        (createFetcher)
import Query.QueryProcessor   (createQueryProcessor)
import Registry               (createRegistry)
import Metadata               (createMetadataApi)
import Types                  (CollectionName, Logger (..), parseCollectionName)
import WarcFileReader         (createWarcFileReader)
import WarcFileWriter         (createWarcFileWriter)

import Control.Concurrent.Async (async, waitAny)
import Control.Monad            (filterM)
import Data.ByteString.Char8    (ByteString, unpack)
import System.Directory         (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import Text.Printf              (printf)

main :: IO ()
main = do

    env <- loadEnvironment

    let logger = stdoutLogger

    registry <- createRegistry env
                               (logger RegistryLogger)

    let warcReader = createWarcFileReader 128
                                          (logger WarcFileReaderLogger)

    let warcWriter = createWarcFileWriter

    let metadataApi = createMetadataApi warcReader

    let queryProcessor = createQueryProcessor env
                                              registry
                                              metadataApi
                                              (logger QueryProcessorLogger)

    let compactor = createCompactor env
                                    registry
                                    warcWriter
                                    metadataApi
                                    (logger CompactorLogger)

    let importer = createImporter env
                                  registry

    let indexer = createIndexer env
                                warcReader
                                warcWriter
                                metadataApi
                                registry

    fetcher <- createFetcher (proxySetting env)

    collections <- findRegistrableCollections env

    if null collections
        then putStrLn "No existing collections found"
        else do
            putStrLn "Found existing collections: "
            mapM_ (\c -> putStr "  " >> print c) collections

    mapM_ (importCollection importer) collections

    printf "Environment is: %s\n" (show env)

    acpc <- async $ runCompactor compactor

    actl <- async $ runController compactor
                                  env
                                  indexer
                                  fetcher
                                  queryProcessor
                                  warcReader
                                  registry
                                  (stdoutLogger ControllerLogger)

    (_, a) <- waitAny [acpc, actl]
    putStrLn a

findRegistrableCollections :: Environment -> IO [CollectionName]
findRegistrableCollections env = do
    let collectionsPath = collectionsDir env
    createDirectoryIfMissing True collectionsPath
    collectionFiles <- listDirectory collectionsPath
    collections     <- filterM (\c -> doesDirectoryExist (collectionsPath <> "/" <> c)) collectionFiles
    mapM parseCollectionName collections

stdoutLogger :: Logger -> ByteString -> IO ()
stdoutLogger logger msg = putStrLn $ printf "%s: %s" (show logger) (unpack msg)
