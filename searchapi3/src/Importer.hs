{-# LANGUAGE OverloadedStrings #-}

module Importer ( Importer (..) 
                , createImporter ) where

import Compactor 
import Component
import Environment   ( Environment (..) )
import ImporterTypes
import Logger        ( Logger (..) )
import Registry      ( Registry, registerInPlace )
import Types

import Control.Concurrent.STM    (atomically)
import Control.Exception.Safe    (catchIO)
import Control.Monad             (unless, when)
import Data.Aeson                (eitherDecodeStrict')
import Data.ByteString.Char8     (ByteString, pack)
import Data.Either               (partitionEithers)
import GHC.IO.Exception          (ExitCode (..))
import System.Directory          (canonicalizePath, getDirectoryContents)
import System.Process.ByteString (readProcessWithExitCode)

newtype Importer =
    Importer { importCollection :: CollectionName -> IO (Either ByteString ())
             }

createImporter :: Environment -> Registry -> Compactor -> Logger -> Importer
createImporter env reg cpc logger = 
    Importer { importCollection = importCollectionImpl env reg cpc logger }

importCollectionImpl :: Environment
                     -> Registry
                     -> Compactor
                     -> Logger
                     -> CollectionName
                     -> IO (Either ByteString ())
importCollectionImpl env registry compactor logger collectionName = do

    let name = getCollectionPath env collectionName

    -- TODO better checking
    subdirs <- map (\p -> concat [name, "/", p])
             . filter (\d -> length d == 36)
           <$> getDirectoryContents name
           >>= mapM canonicalizePath

    (failures, components) <- partitionEithers <$> mapM loadComponent subdirs

    unless (null failures)
           $ do putStrLn "WARNING, FAILED IMPORTS"
                mapM_ (infoBs logger . (\x -> [x])) failures

    mapM_ (atomically . registerInPlace registry collectionName) components

    unless (null failures) loadingCompaction

    pure $ Right ()

    where
    loadingCompaction = do
      putStr "Loading compaction: "
      progress <- compact compactor collectionName
      print progress
      when progress loadingCompaction

    loadComponent :: FilePath -> IO (Either ByteString Component)
    loadComponent componentPath =
        let job = do let execparams = [ "num_docs"
                                      , componentPath
                                      ]

                     (exitcode, stdout, stderr) <- readProcessWithExitCode (indexerBinary env) execparams ""

                     case exitcode of
                       ExitSuccess ->
                         case eitherDecodeStrict' stdout of
                           Left e  -> pure . Left $ pack e
                           Right r -> Right <$> createComponent (num_docs r) componentPath
                       _ -> pure $ Left stderr
            handle ioe = pure . Left . pack $ show ioe
        in catchIO job handle