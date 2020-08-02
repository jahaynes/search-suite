{-# LANGUAGE LambdaCase #-}

module Environment ( Environment (..)
                   , loadEnvironment ) where

import Control.Monad      (unless, when)
import System.Directory   (doesFileExist)
import System.Environment (lookupEnv)
import Text.Printf        (printf)

data Environment =
    Environment { collectionsDir :: !FilePath
                , indexerBinary  :: !FilePath
                }

loadEnvironment :: IO Environment
loadEnvironment = Environment <$> env False "COLLECTIONS_DIR"
                              <*> env True  "INDEXER_BINARY"

env :: Bool -> String -> IO FilePath
env mustExist var =

    lookupEnv var >>= \case

        Nothing -> error $ printf "FATAL: %s not set." var

        Just [] -> error $ printf "FATAL: %s empty." var

        Just target -> do when mustExist (checkExists target)
                          pure target

    where
    checkExists idxbin = do
        exists <- doesFileExist idxbin
        unless exists $
            error $ printf "FATAL: %s does not exist - %s" var idxbin
