{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Registry ( Registry (..)
                , createRegistry
                ) where

import Component
import Environment (Environment (..))
import Types

import           Control.Concurrent.STM   (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, retry)
import           Control.Monad            (forM_)
import           Data.ByteString.Char8    (ByteString)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe               (fromMaybe)
import           Data.Set                 (Set)
import qualified Data.Set as S
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import           Debug.Trace              (trace)
import qualified StmContainers.Set as STM
import           System.Directory         (canonicalizePath, copyFile, createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)

{- TODO fix the 'collections' and 'locks' representations
    Collections should be able to run independently

    TODO use withLock and exception handling, rather than take/release?
-}
data Registry =
    Registry { registerFromTmp          :: !(CollectionName -> Component -> IO ())
             , registerInPlace          :: !(CollectionName -> Component -> STM ())
             , releaseLockIO            :: !(Component -> IO ())
             , takeLock                 :: !(Component -> STM ())
             , unregister               :: !(CollectionName -> Component -> STM ())
             , listCollections          :: !(IO (Set CollectionName))
             , viewCollectionComponents :: !(CollectionName -> STM (Set Component))

             , totalNumComponents       :: !(IO Int)
             , totalLocksHeld           :: !(IO Int)
             }

newtype Collections =
    Collections (TVar (Map CollectionName (Set Component)))

-- subdivide locks further?  STMap CollectionName Component?
newtype Locks =
    Locks (STM.Set Component)

createRegistry :: Environment
               -> (ByteString -> IO ())
               -> IO Registry
createRegistry env logger = do
    collections <- Collections <$> newTVarIO M.empty
    locks       <- Locks <$> STM.newIO
    pure $ Registry { registerFromTmp          = registerFromTmpImpl env collections
                    , registerInPlace          = registerInPlaceImpl collections
                    , releaseLockIO            = releaseLockIOImpl locks logger
                    , takeLock                 = takeLockImpl locks
                    , unregister               = unregisterImpl collections
                    , listCollections          = atomically $ listCollectionsImpl collections
                    , viewCollectionComponents = viewCollectionComponentsImpl collections

                    , totalNumComponents       = atomically $ totalNumComponentsImpl collections
                    , totalLocksHeld           = atomically $ totalLocksHeldImpl locks
                    }

registerFromTmpImpl :: Environment -> Collections -> CollectionName -> Component -> IO ()
registerFromTmpImpl env collections collectionName component = do
    to <- moveDir
    component' <- createComponent (numDocs component) to
    atomically $ registerInPlaceImpl collections collectionName component'
    where
    moveDir = do    -- TODO name properly
        createDirectoryIfMissing True (getCollectionPath env collectionName)
        from <- canonicalizePath $ path component
        uuid <- U.toString <$> U.nextRandom
        let to = concat [getCollectionPath env collectionName, "/", uuid]
        
        -- renamePath from to -- struggles when paths are not on same device
        createDirectoryIfMissing True to
        filesToCopy <- listDirectory from
        forM_ filesToCopy $ \ftc ->
            let fromFile = concat [from, "/", ftc]
                toFile   = concat [  to, "/", ftc]
            in copyFile fromFile toFile
        removeDirectoryRecursive from
        pure to

registerInPlaceImpl :: Collections -> CollectionName -> Component -> STM ()
registerInPlaceImpl (Collections cs) collectionName component =
    modifyTVar' cs $! insert component
    where
    insert c = M.alter (Just . f) collectionName 
        where
        f (Just components) = S.insert c components
        f Nothing           = S.singleton c

releaseLockIOImpl :: Locks
                  -> (ByteString -> IO ())
                  -> Component
                  -> IO ()
releaseLockIOImpl locks logger component = do
    x <- atomically $ releaseLock locks component
    case x of
        Left err -> logger err
        Right _  -> pure ()

-- generify?
takeLockImpl :: Locks -> Component -> STM ()
takeLockImpl (Locks locks) component =
    STM.lookup component locks >>= \case
        True  -> trace "STM retried" retry
        False -> STM.insert component locks

unregisterImpl :: Collections -> CollectionName -> Component -> STM ()
unregisterImpl (Collections cs) collectionName component =
    modifyTVar' cs $! delete component

    where
    delete c = M.alter f collectionName 
        where
        f (Just components) = let components' = S.delete c components
                              in if S.null components' then Nothing else Just components'
        f Nothing           = Nothing

listCollectionsImpl :: Collections -> STM (Set CollectionName)
listCollectionsImpl (Collections cs) = M.keysSet <$> readTVar cs

viewCollectionComponentsImpl :: Collections -> CollectionName -> STM (Set Component)
viewCollectionComponentsImpl (Collections cs) collectionName =
    fromMaybe S.empty . M.lookup collectionName <$> readTVar cs

totalNumComponentsImpl :: Collections -> STM Int
totalNumComponentsImpl (Collections cs) =
    sum . map S.size . map snd . M.toList <$> readTVar cs

totalLocksHeldImpl :: Locks -> STM Int
totalLocksHeldImpl (Locks locks) = STM.size locks

releaseLock :: Locks -> Component -> STM (Either ByteString ())
releaseLock (Locks locks) component =
    STM.lookup component locks >>= \case
        True  -> Right <$> STM.delete component locks
        False -> pure (Left "WARN! Lock was not held")
