{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller ( runController )

import Environment ( Environment (..)
                   , loadEnvironment )

import Service ( Service (..)
               , createService )

import Types ( CollectionName
             , Logger (..)
             , parseCollectionName )

import Control.Monad         (filterM)
import Data.ByteString.Char8 (ByteString, unpack)
import Text.Printf           (printf)
import System.Directory      (createDirectoryIfMissing, doesDirectoryExist, listDirectory)

main :: IO ()
main = do

    env <- loadEnvironment

    service <- createService env stdoutLogger

    collections <- findRegistrableCollections env

    mapM_ (importCollection service) collections

    runController service (stdoutLogger ControllerLogger)

findRegistrableCollections :: Environment -> IO [CollectionName]
findRegistrableCollections env = do
    let collectionsPath = collectionsDir env
    createDirectoryIfMissing True collectionsPath
    collectionFiles <- listDirectory collectionsPath
    collections     <- filterM (\c -> doesDirectoryExist (collectionsPath <> "/" <> c)) collectionFiles
    mapM parseCollectionName collections

stdoutLogger :: Logger -> ByteString -> IO ()
stdoutLogger logger msg = putStrLn $ printf "%s: %s" (show logger) (unpack msg)
