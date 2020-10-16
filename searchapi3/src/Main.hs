{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compactor      (createCompactor)
import Controller     (runController)
import Environment    (Environment (..), loadEnvironment)
import Importer       (Importer (importCollection), createImporter)
import Indexer        (createIndexer)
import QueryProcessor (createQueryProcessor)
import Registry       (createRegistry)
import Types          (CollectionName, Logger (..), parseCollectionName)
import WarcFileReader (createWarcFileReader)
import WarcFileWriter (createWarcFileWriter)

import Control.Monad         (filterM)
import Data.ByteString.Char8 (ByteString, unpack)
import Text.Printf           (printf)
import System.Directory      (createDirectoryIfMissing, doesDirectoryExist, listDirectory)

main :: IO ()
main = do

    env <- loadEnvironment

    let logger = stdoutLogger

    registry <- createRegistry env
                               (logger RegistryLogger)

    let warcReader = createWarcFileReader 128
                                          (logger WarcFileReaderLogger)

    let warcWriter = createWarcFileWriter

    let queryProcessor = createQueryProcessor env
                                              registry
                                              warcReader
                                              (logger QueryProcessorLogger)

    let compactor = createCompactor env
                                    registry
                                    warcWriter
                                    (logger CompactorLogger)

    let importer = createImporter env
                                  registry
                                  compactor

    let indexer = createIndexer env
                                warcReader
                                warcWriter
                                compactor
                                registry

    collections <- findRegistrableCollections env

    if null collections
        then putStrLn "No existing collections found"
        else do
            putStrLn "Found existing collections: "
            mapM_ (\c -> putStr "  " >> print c) collections

    mapM_ (importCollection importer) collections

    printf "Environment is: %s\n" (show env)

    runController compactor
                  indexer
                  queryProcessor
                  registry
                  (stdoutLogger ControllerLogger)

findRegistrableCollections :: Environment -> IO [CollectionName]
findRegistrableCollections env = do
    let collectionsPath = collectionsDir env
    createDirectoryIfMissing True collectionsPath
    collectionFiles <- listDirectory collectionsPath
    collections     <- filterM (\c -> doesDirectoryExist (collectionsPath <> "/" <> c)) collectionFiles
    mapM parseCollectionName collections

stdoutLogger :: Logger -> ByteString -> IO ()
stdoutLogger logger msg = putStrLn $ printf "%s: %s" (show logger) (unpack msg)
