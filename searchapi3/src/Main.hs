{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compactor              (createCompactor)
import Controller             (runController)
import EnvironmentShim        (getCollectionsPathImpl, getProxySettingImpl)
import Importer               (Importer (importCollection), createImporter)
import Indexer                (createIndexer)
import Network.Fetcher        (createFetcher)
import QueryProcessor         (createQueryProcessor)
import Registry               (createRegistry)
import Metadata               (createMetadataApi)
import Types                  (CollectionName, Logger (..), parseCollectionName)
import WarcFileReader         (createWarcFileReader)
import WarcFileWriter         (createWarcFileWriter)

import Control.Monad         (filterM)
import Data.ByteString.Char8 (ByteString, unpack)
import System.Directory      (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import Text.Printf           (printf)

main :: IO ()
main = do

    let logger = stdoutLogger

    registry <- createRegistry (logger RegistryLogger)

    let warcReader = createWarcFileReader 128
                                          (logger WarcFileReaderLogger)

    let warcWriter = createWarcFileWriter

    let metadataApi = createMetadataApi warcReader

    let queryProcessor = createQueryProcessor registry
                                              metadataApi
                                              (logger QueryProcessorLogger)

    let compactor = createCompactor registry
                                    warcWriter
                                    metadataApi
                                    (logger CompactorLogger)

    let importer = createImporter registry
                                  compactor

    let indexer = createIndexer warcReader
                                warcWriter
                                metadataApi
                                compactor
                                registry

    fetcher <- createFetcher =<< getProxySettingImpl

    collections <- findRegistrableCollections

    if null collections
        then putStrLn "No existing collections found"
        else do
            putStrLn "Found existing collections: "
            mapM_ (\c -> putStr "  " >> print c) collections

    mapM_ (importCollection importer) collections

    runController compactor
                  indexer
                  fetcher
                  queryProcessor
                  warcReader
                  registry
                  (stdoutLogger ControllerLogger)

findRegistrableCollections :: IO [CollectionName]
findRegistrableCollections = do
    collectionsPath <- getCollectionsPathImpl
    createDirectoryIfMissing True collectionsPath
    collectionFiles <- listDirectory collectionsPath
    collections     <- filterM (\c -> doesDirectoryExist (collectionsPath <> "/" <> c)) collectionFiles
    mapM parseCollectionName collections

stdoutLogger :: Logger -> ByteString -> IO ()
stdoutLogger logger msg = putStrLn $ printf "%s: %s" (show logger) (unpack msg)

{-

    Restore these preflight checks somewhere

loadEnvironment :: IO Environment
loadEnvironment = do

    colnsDir <- getCollectionsPathImpl
    createDirectoryIfMissing True colnsDir

    idxbin <- getIndexerBinaryImpl
    exists <- doesFileExist idxbin
    unless exists $
        error $ printf "FATAL: %s does not exist\n" idxbin

    proxy <- getProxySettingImpl

    pure Environment { collectionsDir = colnsDir
                     , indexerBinary  = idxbin
                     , proxySetting   = proxy
                     }


-}