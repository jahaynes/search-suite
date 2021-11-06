{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Controllers.Collections where

import Compactor   (Compactor (mergeInto))
import Environment (Environment (..))
import Registry    (Registry (..))
import Types       (CollectionName (..))

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


collectionsServer :: Compactor
                  -> Environment
                  -> Registry
                  -> ServerT CollectionsApi IO 
collectionsServer compactor env registry
    = listCollections registry
 :<|> deleteCollection env registry
 :<|> mergeInto compactor

deleteCollection :: Environment
                 -> Registry
                 -> CollectionName
                 -> IO ()
deleteCollection env registry collectionName@(CollectionName cn) = do

    cs <- atomically $ do
        components <- viewCollectionComponents registry collectionName
        mapM_ (takeLock registry) components
        mapM_ (unregister registry collectionName) components
        pure components

    let collectionPath = collectionsDir env <> "/" <> cn

    removeDirectoryRecursive collectionPath

    mapM_ (releaseLockIO registry) cs

