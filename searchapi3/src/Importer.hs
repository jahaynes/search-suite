{-# LANGUAGE OverloadedStrings #-}

module Importer ( Importer (..) 
                , createImporter ) where

import Compactor 
import Component
import EnvironmentShim ( getCollectionPathImpl, getIndexerBinaryImpl )
import ImporterTypes
import Registry        ( Registry, registerInPlace )
import Types

import Control.Concurrent.STM    (atomically)
import Control.Exception.Safe    (catchIO)
import Control.Monad             (unless, when)
import Data.Aeson                (eitherDecodeStrict')
import Data.ByteString.Char8     (ByteString, unpack)
import Data.Either               (partitionEithers)
import GHC.IO.Exception          (ExitCode (..))
import System.Directory          (canonicalizePath, getDirectoryContents)
import System.Process.ByteString (readProcessWithExitCode)

newtype Importer =
    Importer { importCollection :: CollectionName -> IO (Either ByteString ())
             }

createImporter :: Registry -> Compactor -> Importer
createImporter reg cpc = 
    Importer { importCollection = importCollectionImpl reg cpc}

importCollectionImpl :: Registry
                     -> Compactor
                     -> CollectionName
                     -> IO (Either ByteString ())
importCollectionImpl registry compactor collectionName = do

    name <- getCollectionPathImpl collectionName

    -- TODO better checking
    subdirs <- map (\p -> concat [name, "/", p])
             . filter (\d -> length d == 36)
           <$> getDirectoryContents name
           >>= mapM canonicalizePath

    (failures, components) <- partitionEithers <$> mapM loadComponent subdirs

    unless (null failures)
           $ do putStrLn "WARNING, FAILED IMPORTS"
                mapM_ print failures

    mapM_ (atomically . registerInPlace registry collectionName) components

    unless (null failures) loadingCompaction

    pure $ Right ()

    where
    loadingCompaction = do
      putStr "Loading compaction: "
      progress <- compact compactor collectionName
      print progress
      when progress loadingCompaction

    loadComponent :: FilePath -> IO (Either String Component)
    loadComponent componentPath = do

        bin <- getIndexerBinaryImpl

        let job = do let execparams = [ "num_docs"
                                      , componentPath
                                      ]

                     (exitcode, stdout, stderr) <- readProcessWithExitCode bin execparams ""

                     case exitcode of
                       ExitSuccess ->
                         case eitherDecodeStrict' stdout of
                           Left e -> pure $ Left e
                           Right r -> Right <$> createComponent (num_docs r) componentPath
                       _ -> pure $ Left (unpack stderr)
            handle ioe = pure $ Left (show ioe)
        catchIO job handle