{-# LANGUAGE LambdaCase #-}

module EnvironmentShim ( getCollectionPathImpl
                       , getCollectionsPathImpl
                       , getIndexerBinaryImpl
                       , getProxySettingImpl
                       ) where

import Environment (Env (..))
import Types       (CollectionName (..))

import Control.Monad.Trans.Reader
import Data.ByteString.Char8  (ByteString)

getCollectionPathImpl :: Monad m => CollectionName -> ReaderT Env m FilePath
getCollectionPathImpl (CollectionName name) = do
    colns <- getCollectionsPathImpl
    pure $ colns <> "/" <> name

getCollectionsPathImpl :: Monad m => ReaderT Env m FilePath
getCollectionsPathImpl = collectionsPath <$> ask

{-
getCollectionsPathImpl :: IO FilePath
getCollectionsPathImpl =
    lookupEnv "COLLECTIONS_DIR" >>= \case
        Just colnsDir -> makeAbsolute colnsDir
        Nothing -> do
            putStrLn "COLLECTIONS_DIR undefined, defaulting to 'collections'"
            pure "collections"
-}

-- TODO log on Nothing case
getIndexerBinaryImpl :: Monad m => ReaderT Env m FilePath
getIndexerBinaryImpl = indexerBinary <$> ask

{-
getIndexerBinaryImpl :: IO FilePath
getIndexerBinaryImpl =
    lookupEnv "INDEXER_BINARY" >>= \case
        Just idxbin -> pure idxbin
        Nothing -> do
            putStrLn "INDEXER_BINARY undefined, defaulting to 'bin/indexer-qp2'"
            pure "bin/indexer-qp2"
-}

getProxySettingImpl :: Monad m => ReaderT Env m (Maybe (ByteString, Int))
getProxySettingImpl = proxySetting <$> ask

{-
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
-}