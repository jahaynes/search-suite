{-# LANGUAGE LambdaCase #-}

module Environment ( Environment (..)
                   , loadEnvironment ) where

import Control.Monad      (unless)
import System.Directory   (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import Text.Printf        (printf)

data Environment =
    Environment { collectionsDir :: !FilePath
                , indexerBinary  :: !FilePath
                } deriving Show

loadEnvironment :: IO Environment
loadEnvironment = do

    colnsDir <- lookupEnv "COLLECTIONS_DIR" >>= \case
        Just colnsDir -> pure colnsDir
        Nothing -> do
            putStrLn "COLLECTIONS_DIR undefined, defaulting to 'collections'"
            pure "collections"

    createDirectoryIfMissing True colnsDir

    idxbin <- lookupEnv "INDEXER_BINARY" >>= \case
        Just idxbin -> pure idxbin
        Nothing -> do
            putStrLn "INDEXER_BINARY undefined, defaulting to 'bin/indexer-qp2'"
            pure "bin/indexer-qp2"

    exists <- doesFileExist idxbin
    unless exists $
        error $ printf "FATAL: %s does not exist\n" idxbin

    pure Environment { collectionsDir = colnsDir
                     , indexerBinary  = idxbin
                     }