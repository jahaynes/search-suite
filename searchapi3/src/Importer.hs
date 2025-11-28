{-# LANGUAGE OverloadedStrings #-}

module Importer ( Importer (..) 
                , createImporter ) where

import Component
import Environment ( Environment (..) )
import Registry    ( Registry, registerInPlace )

import ImporterTypes
import Types

import Control.Concurrent.STM    (atomically)
import Control.Exception.Safe    (catchIO)
import Control.Monad             (unless)
import Data.Aeson                (eitherDecodeStrict')
import Data.ByteString.Char8     (ByteString, unpack)
import Data.Either               (partitionEithers)
import GHC.IO.Exception          (ExitCode (..))
import System.Directory          (canonicalizePath, getDirectoryContents)
import System.Process.ByteString (readProcessWithExitCode)

newtype Importer =
    Importer { importCollection :: CollectionName -> IO (Either ByteString ())
             }

createImporter :: Environment -> Registry -> Importer
createImporter env reg = 
    Importer { importCollection = importCollectionImpl env reg }

importCollectionImpl :: Environment
                     -> Registry
                     -> CollectionName
                     -> IO (Either ByteString ())
importCollectionImpl env registry collectionName = do

    let name = getCollectionPath env collectionName

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
    pure $ Right ()

    where
    loadComponent :: FilePath -> IO (Either String Component)
    loadComponent componentPath =
        let job = do let execparams = [ "num_docs"
                                      , componentPath
                                      ]

                     (exitcode, stdout, stderr) <- readProcessWithExitCode (indexerBinary env) execparams ""

                     case exitcode of
                       ExitSuccess ->
                         case eitherDecodeStrict' stdout of
                           Left e -> pure $ Left e
                           Right r -> Right <$> createComponent (num_docs r) componentPath
                       _ -> pure $ Left (unpack stderr)
            handle ioe = pure $ Left (show ioe)
        in catchIO job handle