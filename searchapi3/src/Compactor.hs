{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Compactor ( Compactor (..)
                 , createCompactor
                 ) where

import           CompactorStrategy ( hybridStrategy )

import           Component         ( Component (..)
                                   , createComponent )

import           Environment       ( Environment (indexerBinary) )

import           Registry          ( Registry (..) )

import           Metadata          ( MetadataApi (..) )

import           Types             ( CollectionName (..)
                                   , getCollectionPath
                                   , numDocs
                                   , path )

import           WarcFileWriter ( WarcFileWriter (..) )

import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM      (STM, atomically)
import           Control.Exception.Safe      (catchIO)  -- todo remove
import           Control.Monad               (forever, forM_, unless)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set              as S
import qualified Data.UUID             as U
import qualified Data.UUID.V4          as U
import           System.Directory            (canonicalizePath, createDirectoryIfMissing, removeDirectoryRecursive)
import           System.Process              (callProcess)
import           UnliftIO.Exception          (bracket)

data Compactor =
    Compactor { _compact     :: CollectionName -> IO Bool
              , runCompactor :: IO String
              , mergeInto    :: CollectionName -> CollectionName -> IO ()
              }

createCompactor :: Environment
                -> Registry
                -> WarcFileWriter
                -> MetadataApi
                -> (ByteString -> IO ())
                -> Compactor
createCompactor env registry wfw metadataApi logger =
    Compactor { _compact     = compactImpl   env registry wfw metadataApi logger
              , mergeInto    = mergeIntoImpl env registry wfw metadataApi logger
              , runCompactor = runCompactorImpl env registry wfw metadataApi logger
              }

chooseAndLockComponents :: Registry
                        -> CollectionName
                        -> STM (Maybe (Component, Component))
chooseAndLockComponents registry collectionName = do
    componentSet <- listComponents registry collectionName
    case hybridStrategy componentSet of
        Nothing -> pure Nothing
        Just (_reason, x, y) -> do
            takeLock registry x
            takeLock registry y
            pure $ Just (x, y)

runCompactorImpl :: Environment
                 -> Registry
                 -> WarcFileWriter
                 -> MetadataApi
                 -> (ByteString -> IO ())
                 -> IO String
runCompactorImpl env registry warcFileWriter metadataApi logger = do

    forever $ do

        threadDelay 100  -- Make this sleep better using STM

        colns <- atomically $ listCollections registry

        forM_ colns $ \col -> do

            mLocked <- atomically $ do

                -- TODO resource-safety / release locks on error
                unlocked <- listUnlockedComponents registry col
                case hybridStrategy unlocked of
                    Nothing -> pure Nothing
                    Just (_, x, y) -> do
                        takeLock registry x
                        takeLock registry y
                        pure $ Just (Locked x, Locked y)

            case mLocked of
                Nothing -> pure ()
                Just (Locked x, Locked y) -> do
                    todo <- mergeLocked env registry warcFileWriter metadataApi logger col (Locked x) (Locked y)
                    releaseLockIO registry x
                    releaseLockIO registry y

    pure "Compactor stopped unexpectedly"

newtype Locked a
    = Locked a

mergeLocked :: Environment
            -> Registry
            -> WarcFileWriter
            -> MetadataApi
            -> (ByteString -> IO ())
            -> CollectionName
            -> Locked Component
            -> Locked Component
            -> IO (Either ByteString ())
mergeLocked env registry warcFileWriter metadataApi logger collectionName lx@(Locked x) ly@(Locked y) =

    mergeComponentFiles env warcFileWriter metadataApi (indexerBinary env) collectionName lx ly logger >>= \case

        Left errMsg -> do
            pure $ Left errMsg

        Right z -> do
            atomically $ do
                unregister      registry collectionName x
                unregister      registry collectionName y
                registerInPlace registry collectionName z
                listComponents registry collectionName
            removeDirectoryRecursive (path x)
            removeDirectoryRecursive (path y)
            pure $ Right ()

-- Deprecating this.  Lock taking should be taken before this
compactImpl :: Environment
            -> Registry
            -> WarcFileWriter
            -> MetadataApi
            -> (ByteString -> IO ())
            -> CollectionName
            -> IO Bool
compactImpl env registry warcFileWriter metadataApi logger collectionName@(CollectionName cn) =

     bracket (atomically $ chooseAndLockComponents registry collectionName)

             (\mLocked -> case mLocked of
                             Nothing -> pure ()
                             Just (x, y) -> mapM_ (releaseLockIO registry) [x, y])

             (\mLocked -> case mLocked of
                             Nothing -> pure False
                             Just (x, y) -> do
                                 logger $ "Took locks: " <> C8.pack (show (cmp_filePath x)) <> " " <> C8.pack (show (cmp_filePath y))
                                 mergeLocked env registry warcFileWriter metadataApi logger collectionName (Locked x) (Locked y) >>= \case
                                    Left errMsg -> do
                                        logger errMsg
                                        error $ C8.unpack errMsg
                                    Right () ->
                                        pure True)

-- TODO (critical - old directory can be removed on failure)
    -- Components are left in mem?
-- TODO logging and exceptions
mergeIntoImpl :: Environment
              -> Registry
              -> WarcFileWriter
              -> MetadataApi
              -> (ByteString -> IO ())
              -> CollectionName
              -> CollectionName
              -> IO ()
mergeIntoImpl env reg wfw metadataApi logger dest src = do

    -- Check it has at least one component
    components <- atomically $ listComponents reg src
    unless (S.null components) loop

    where
    loop = do
        -- (Try to) find and unregister next source component
        mComponent <- atomically $ do
            cmps <- listComponents reg src
            case S.minView cmps of
                Nothing     -> pure Nothing
                Just (c, _) -> do
                    unregister reg src c
                    pure $ Just c

        case mComponent of

            -- No components - remove directory
            Nothing -> removeDirectoryRecursive (getCollectionPath env src)

            -- If successful
            Just component -> do

                -- Register into destination
                registerFromTmp reg dest component

                -- Compact
                _ <- compactImpl env reg wfw metadataApi logger dest

                -- Keep going
                loop

mergeComponentFiles :: Environment
                    -> WarcFileWriter
                    -> MetadataApi
                    -> FilePath
                    -> CollectionName
                    -> Locked Component
                    -> Locked Component
                    -> (ByteString -> IO ())
                    -> IO (Either ByteString Component)

mergeComponentFiles env wfw metadataApi indexerPath collectionName (Locked x) (Locked y) logger = do

    let cn = getCollectionPath env collectionName
    createDirectoryIfMissing True cn
    cmpName <- U.toString <$> U.nextRandom
    dest <- canonicalizePath $ concat [cn, "/", cmpName]

    let mergeArgs = [ "merge"
                    , dest
                    , path x
                    , path y ]

                 -- Metrics needed
        job = do callProcess indexerPath mergeArgs

                 interleaveWarcFiles wfw x y dest   -- TODO include lock types

                 mergeMetadata metadataApi (path x) (path y) dest

                 Right <$> createComponent (numDocs x + numDocs y) dest

        handle ioe = do
            let errMsg = "Merge failed.  Params were " ++ show mergeArgs
            logger $ C8.pack errMsg
            error errMsg

    catchIO job handle
