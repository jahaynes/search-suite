{-# LANGUAGE LambdaCase #-}

module Importer ( Importer (..) 
                , createImporter ) where

import Bin           ( Bin (..), runJson )
import Compactor
import Component
import Environment   ( Environment (..) )
import ImporterTypes ( NumDocsReply (num_docs) )
import Logger        ( Logger (..) )
import Registry      ( Registry, registerInPlace )
import Types

import Control.Concurrent.STM (atomically)
import Control.Monad          (unless, when)
import Data.Either            (partitionEithers)
import Data.Text              (Text)
import System.Directory       (canonicalizePath, getDirectoryContents)

newtype Importer =
    Importer { importCollection :: CollectionName -> IO (Either [Text] ())
             }

createImporter :: Environment -> Registry -> Compactor -> Logger -> Importer
createImporter env reg cpc logger = 
    Importer { importCollection = importCollectionImpl env reg cpc logger }

importCollectionImpl :: Environment
                     -> Registry
                     -> Compactor
                     -> Logger
                     -> CollectionName
                     -> IO (Either [Text] ())
importCollectionImpl env registry compactor logger collectionName = do

    let name = getCollectionPath env collectionName

    -- TODO better checking
    subdirs <- map (\p -> concat [name, "/", p])
             . filter (\d -> length d == 36)
           <$> getDirectoryContents name
           >>= mapM canonicalizePath

    (failuress, components) <- partitionEithers <$> mapM loadComponent subdirs

    let failures = concat failuress

    unless (null failures)
           $ do putStrLn "WARNING, FAILED IMPORTS"
                mapM_ (info logger . (\x -> [x])) failures

    mapM_ (atomically . registerInPlace registry collectionName) components

    unless (null failures) loadingCompaction

    pure $ Right ()

    where
    loadingCompaction = do
        putStr "Loading compaction: "
        progress <- compact compactor collectionName
        print progress
        when progress loadingCompaction

    loadComponent :: FilePath -> IO (Either [Text] Component)
    loadComponent componentPath =

        let bin = Bin { getCmd   = indexerBinary env
                      , getArgs  = [ "num_docs" , componentPath ]
                      , getInput = Nothing }

        in runJson bin >>= \case

               Left l ->
                   pure $ Left l

               Right (stderr, numDocsReply) ->
                   Right <$> createComponent (num_docs numDocsReply) componentPath
