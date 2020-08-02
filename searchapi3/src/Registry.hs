{-# LANGUAGE OverloadedStrings #-}

module Registry ( Registry (..)
                , createRegistry
                ) where

import Component
import Environment (Environment (..))
import Types

import           Control.Concurrent.STM     (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import           Control.Monad              (forM_)
import           Data.ByteString.Char8      (ByteString)
import           Data.Map                   (Map)
import qualified Data.Map as M
import           Data.Maybe                 (fromMaybe)
import           Data.Set                   (Set)
import qualified Data.Set as S
import qualified Data.UUID             as U
import qualified Data.UUID.V4          as U
import           Debug.Trace                (trace)
import           System.Directory           (canonicalizePath, copyFile, createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)

{- TODO fix the 'collections' and 'locks' representations
    Collections should be able to run independently
-}
data Registry =
    Registry { registerFromTmp          :: CollectionName -> Component -> IO ()
             , registerInPlace          :: CollectionName -> Component -> STM ()
             , releaseLockIO            :: Component -> IO ()   -- TODO can be scoped to CollectionName
             , takeLock                 :: Component -> STM ()  -- TODO can be scoped to CollectionName
             , unregister               :: CollectionName -> Component -> STM ()
             , viewCollectionComponents :: CollectionName -> STM (Set Component)

             , totalNumComponents       :: IO Int
             , totalLocksHeld           :: IO Int
             }

newtype Collections =
    Collections (TVar (Map CollectionName (Set Component)))

newtype Locks =
    Locks (TVar (Set Component))

createRegistry :: Environment
               -> (ByteString -> IO ())
               -> IO Registry
createRegistry env logger = do
    collections <- Collections <$> newTVarIO M.empty
    locks       <- Locks <$> newTVarIO S.empty
    pure $ Registry { registerFromTmp          = registerFromTmpImpl env collections
                    , registerInPlace          = registerInPlaceImpl collections
                    , releaseLockIO            = releaseLockIOImpl locks logger
                    , takeLock                 = takeLockImpl locks
                    , unregister               = unregisterImpl collections
                    , viewCollectionComponents = viewCollectionComponentsImpl collections

                    , totalNumComponents       = totalNumComponentsImpl collections
                    , totalLocksHeld           = totalLocksHeldImpl locks
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
releaseLockIOImpl locks logger c = do
    x <- atomically $ releaseLock locks c
    case x of
        Left err -> logger err
        Right _  -> pure ()

takeLockImpl :: Locks -> Component -> STM ()
takeLockImpl (Locks locks) component = do
    locks' <- readTVar locks
    if S.member component locks'
        then trace "STM retried" retry
        else writeTVar locks $! S.insert component locks'

unregisterImpl :: Collections -> CollectionName -> Component -> STM ()
unregisterImpl (Collections cs) collectionName component =
    modifyTVar' cs $! delete component

    where
    delete c = M.alter f collectionName 
        where
        f (Just components) = let components' = S.delete c components
                              in if S.null components' then Nothing else Just components'
        f Nothing           = Nothing

viewCollectionComponentsImpl :: Collections -> CollectionName -> STM (Set Component)
viewCollectionComponentsImpl (Collections cs) collectionName =
    fromMaybe S.empty . M.lookup collectionName <$> readTVar cs

totalNumComponentsImpl :: Collections -> IO Int
totalNumComponentsImpl (Collections cs) =
    atomically (sum . map S.size . map snd . M.toList <$> readTVar cs)

totalLocksHeldImpl :: Locks -> IO Int
totalLocksHeldImpl (Locks locks) =
    atomically (S.size <$> readTVar locks)

releaseLock :: Locks -> Component -> STM (Either ByteString ())
releaseLock (Locks locks) component = do
    locks' <- readTVar locks
    if S.member component locks'
        then Right <$> writeTVar locks (S.delete component locks')
        else pure (Left "WARN! Lock was not held")

