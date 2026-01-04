{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Collections where

import Compactor       (Compactor (mergeInto))
import EnvironmentShim (getCollectionsPathImpl)
import Registry        (Registry (..))
import Types           (CollectionName (..))

import Control.Concurrent.STM (atomically)
import Data.Set               (Set)
import Servant
import System.Directory       (removeDirectoryRecursive)

type CollectionsApi = "collection" :> Get '[JSON] (Set CollectionName)

                 :<|> "collection" :> Capture "col" CollectionName
                                   :> Delete '[JSON] ()

                 :<|> "mergeInto" :> QueryParam' '[Required] "dest" CollectionName
                                  :> QueryParam' '[Required] "src" CollectionName
                                  :> Post '[JSON] ()

                 :<|> "collection-dir" :> Get '[JSON] FilePath

collectionsServer :: Compactor
                  -> Registry
                  -> ServerT CollectionsApi IO 
collectionsServer compactor registry
    = listCollections registry
 :<|> deleteCollection registry
 :<|> mergeInto compactor
 :<|> getCollectionsPathImpl -- TODO not-impl

deleteCollection :: Registry
                 -> CollectionName
                 -> IO ()
deleteCollection registry collectionName@(CollectionName cn) = do

    cs <- atomically $ do
        components <- viewCollectionComponents registry collectionName
        mapM_ (takeLock registry) components
        mapM_ (unregister registry collectionName) components
        pure components

    cd <- getCollectionsPathImpl -- TODO not-impl

    let collectionPath = cd <> "/" <> cn

    removeDirectoryRecursive collectionPath

    mapM_ (releaseLockIO registry) cs
