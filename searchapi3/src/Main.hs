{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compactor                 (createCompactor)
import Controllers.Controller    (runController)
import Environment               (Environment (..), loadEnvironment)
import Extensions.GitIndexer     (createGitIndexer)
import Extensions.WarcIndexer    (createWarcIndexer)
import Importer                  (Importer (importCollection), createImporter)
import Indexer                   (createIndexer)
import Logger                    (LoggerType (..), createLogger)
import Network.Fetcher           (createFetcher)
import Query.QueryProcessor      (createQueryProcessor)
import Query.SpellingProcessor   (createSpellingProcessor)
import Query.StructuredProcessor (createStructuredProcessor)
import Registry                  (createRegistry)
import Metadata                  (createMetadataApi)
import Types                     (CollectionName, parseCollectionName)
import WarcFileReader            (createWarcFileReader)
import WarcFileWriter            (createWarcFileWriter)

import Control.Monad    (filterM)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import Text.Printf      (printf)

main :: IO ()
main = do

    env <- loadEnvironment

    registry <- createRegistry env
                               (createLogger RegistryLogger)

    let warcReader = createWarcFileReader 128
                                          (createLogger WarcFileReaderLogger)

    let warcWriter = createWarcFileWriter

    let metadataApi = createMetadataApi warcReader

    let queryProcessor = createQueryProcessor env
                                              registry
                                              metadataApi
                                              (createLogger QueryProcessorLogger)

    let spellingProcessor = createSpellingProcessor env
                                                    registry
                                                    (createLogger SpellingProcessorLogger)

    let structuredProcessor = createStructuredProcessor env
                                                        registry
                                                        metadataApi
                                                        (createLogger StructuredProcessorLogger)

    let compactor = createCompactor env
                                    registry
                                    warcWriter
                                    metadataApi
                                    (createLogger CompactorLogger)

    let importer = createImporter env
                                  registry
                                  compactor
                                  (createLogger ImporterLogger)

    let indexer = createIndexer env
                                warcWriter
                                metadataApi
                                compactor
                                registry

    let warcIndexer = createWarcIndexer warcReader
                                        indexer
                                        (createLogger WarcIndexerLogger)

    gitIndexer <- createGitIndexer indexer

    fetcher <- createFetcher (proxySetting env)

    collections <- findRegistrableCollections env

    if null collections
        then putStrLn "No existing collections found"
        else do
            putStrLn "Found existing collections: "
            mapM_ (\c -> putStr "  " >> print c) collections

    mapM_ (importCollection importer) collections

    printf "Environment is: %s\n" (show env)

    runController compactor
                  env
                  indexer
                  gitIndexer
                  warcIndexer
                  fetcher
                  queryProcessor
                  spellingProcessor
                  structuredProcessor
                  warcReader
                  registry
                  (createLogger ControllerLogger)

findRegistrableCollections :: Environment -> IO [CollectionName]
findRegistrableCollections env = do
    let collectionsPath = collectionsDir env
    createDirectoryIfMissing True collectionsPath
    collectionFiles <- listDirectory collectionsPath
    collections     <- filterM (\c -> doesDirectoryExist (collectionsPath <> "/" <> c)) collectionFiles
    mapM parseCollectionName collections
