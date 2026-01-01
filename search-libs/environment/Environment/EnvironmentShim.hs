{-# LANGUAGE LambdaCase #-}

module EnvironmentShim ( getCollectionPathImpl
                       , getCollectionsPathImpl
                       , getIndexerBinaryImpl
                       , getProxySettingImpl
                       ) where

import Types (CollectionName (..))

import Data.ByteString.Char8 (ByteString, pack)
import Data.List.Split       (splitOn)
import System.Directory      (makeAbsolute)
import System.Environment    (lookupEnv)
import Text.Read             (readMaybe)

getCollectionPathImpl :: CollectionName -> IO FilePath
getCollectionPathImpl (CollectionName name) = do
    colns <- getCollectionsPathImpl
    pure $ colns <> "/" <> name

getCollectionsPathImpl :: IO FilePath
getCollectionsPathImpl =
    lookupEnv "COLLECTIONS_DIR" >>= \case
        Just colnsDir -> makeAbsolute colnsDir
        Nothing -> do
            putStrLn "COLLECTIONS_DIR undefined, defaulting to 'collections'"
            pure "collections"

getIndexerBinaryImpl :: IO FilePath
getIndexerBinaryImpl =
    lookupEnv "INDEXER_BINARY" >>= \case
        Just idxbin -> pure idxbin
        Nothing -> do
            putStrLn "INDEXER_BINARY undefined, defaulting to 'bin/indexer-qp2'"
            pure "bin/indexer-qp2"

getProxySettingImpl :: IO (Maybe (ByteString, Int))
getProxySettingImpl =
    lookupEnv "PROXY" >>= \case
        Just strProxy ->
            case splitOn ":" strProxy of
                [addr, strPort] ->
                    case readMaybe strPort of
                        Just port -> pure $ Just (pack addr, port)
                        Nothing   -> error "Couldnt understand proxy port"
                _ -> error "Couldn't understand proxy setting"
        Nothing -> pure Nothing
