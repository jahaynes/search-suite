{-# LANGUAGE LambdaCase #-}

module Environment ( Environment (..)
                   , loadEnvironment ) where

import Control.Monad         (unless)
import Data.ByteString.Char8 (ByteString, pack)
import Data.List.Split       (splitOn)
import System.Directory      (createDirectoryIfMissing, doesFileExist)
import System.Environment    (lookupEnv)
import Text.Printf           (printf)
import Text.Read             (readMaybe)

data Environment =
    Environment { collectionsDir :: !FilePath
                , indexerBinary  :: !FilePath
                , proxySetting   :: !(Maybe (ByteString, Int))
                } deriving Show

loadEnvironment :: IO Environment
loadEnvironment = do

    colnsDir <- getColnsDir
    createDirectoryIfMissing True colnsDir

    idxbin <- getIdxBin
    exists <- doesFileExist idxbin
    unless exists $
        error $ printf "FATAL: %s does not exist\n" idxbin

    proxy <- getProxySetting

    pure Environment { collectionsDir = colnsDir
                     , indexerBinary  = idxbin
                     , proxySetting   = proxy
                     }

    where
    getColnsDir =
        lookupEnv "COLLECTIONS_DIR" >>= \case
            Just colnsDir -> pure colnsDir
            Nothing -> do
                putStrLn "COLLECTIONS_DIR undefined, defaulting to 'collections'"
                pure "collections"

    getIdxBin =
        lookupEnv "INDEXER_BINARY" >>= \case
            Just idxbin -> pure idxbin
            Nothing -> do
                putStrLn "INDEXER_BINARY undefined, defaulting to 'bin/indexer-qp2'"
                pure "bin/indexer-qp2"

    getProxySetting =
        lookupEnv "PROXY" >>= \case
            Just strProxy ->
                case splitOn ":" strProxy of
                    [addr, strPort] ->
                        case readMaybe strPort of
                            Just port -> pure $ Just (pack addr, port)
                            Nothing   -> error "Couldnt understand proxy port"
                    _ -> error "Couldn't understand proxy setting"
            Nothing -> pure Nothing
