{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , QuasiQuotes #-}

module Compactor ( Compactor (..)
                 , createCompactor
                 ) where

import Bin               ( Bin (..), runBs )
import CompactorStrategy ( hybridStrategy )
import Component         ( Component (..), createComponent )
import Environment       ( Environment (indexerBinary) )
import Logger            ( Logger (..) )
import Metadata          ( MetadataApi (..) )
import Registry          ( Registry (..) )
import Types             ( CollectionName (..), getCollectionPath, numDocs, path )
import WarcFileWriter    ( WarcFileWriter (..) )

import           Control.Concurrent.STM     (STM, atomically)
import           Control.Monad              (unless)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.Set              as S
import           Data.String.Interpolate    (i)
import qualified Data.UUID             as U
import qualified Data.UUID.V4          as U
import           System.Directory           (canonicalizePath, createDirectoryIfMissing, removeDirectoryRecursive)
import           UnliftIO.Exception         (bracket)

data Compactor =
    Compactor { compact   :: !(CollectionName -> IO Bool)
              , mergeInto :: !(CollectionName -> CollectionName -> IO ())
              }

createCompactor :: Environment
                -> Registry
                -> WarcFileWriter
                -> MetadataApi
                -> Logger
                -> Compactor
createCompactor env registry wfw metadataApi logger =
    Compactor { compact   = compactImpl   env registry wfw metadataApi logger
              , mergeInto = mergeIntoImpl env registry wfw metadataApi logger
              }

chooseAndLockComponents :: Registry
                        -> CollectionName
                        -> STM (Maybe (Component, Component))
chooseAndLockComponents registry collectionName = do
    componentSet <- viewCollectionComponents registry collectionName
    case hybridStrategy componentSet of
        Nothing -> pure Nothing
        Just (_reason, x, y) -> do
            takeLock registry x
            takeLock registry y
            pure $ Just (x, y)

compactImpl :: Environment
            -> Registry
            -> WarcFileWriter
            -> MetadataApi
            -> Logger
            -> CollectionName
            -> IO Bool
compactImpl env registry wfw metadataApi logger collectionName@(CollectionName cn) =

     bracket (atomically $ chooseAndLockComponents registry collectionName)

             (\mLocked -> case mLocked of
                             Nothing -> pure ()
                             Just (x, y) -> mapM_ (releaseLockIO registry) [x, y])

             (\mLocked -> case mLocked of

                             Nothing -> pure False

                             Just (x, y) -> do

                                 infoBs logger [[i|Took locks: #{cmp_filePath x} #{cmp_filePath y}|]]

                                 mergeResult <- mergeComponentFiles env wfw metadataApi collectionName x y logger

                                 case mergeResult of

                                     Left errMsg -> do
                                         infoBs logger errMsg
                                         pure False

                                     Right z -> do
                                         componentSet <- atomically $ do
                                             unregister      registry collectionName x
                                             unregister      registry collectionName y
                                             registerInPlace registry collectionName z
                                             viewCollectionComponents registry collectionName
                                         removeDirectoryRecursive (path x)
                                         removeDirectoryRecursive (path y)
                                         infoBs logger [[i|#{cn} components: #{cmp_size <$> S.toList componentSet}|]]
                                         pure True)

-- TODO (critical - old directory can be removed on failure)
    -- Components are left in mem?
-- TODO logging and exceptions
mergeIntoImpl :: Environment
              -> Registry
              -> WarcFileWriter
              -> MetadataApi
              -> Logger
              -> CollectionName
              -> CollectionName
              -> IO ()
mergeIntoImpl env reg wfw metadataApi logger dest src = do

    -- Check it has at least one component
    components <- atomically $ viewCollectionComponents reg src
    unless (S.null components) loop

    where
    loop = do
        -- (Try to) find and unregister next source component
        mComponent <- atomically $ do
            cmps <- viewCollectionComponents reg src
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

                -- Compact -- TODO - why ignoring?
                _ <- compactImpl env reg wfw metadataApi logger dest

                -- Keep going
                loop

mergeComponentFiles :: Environment
                    -> WarcFileWriter
                    -> MetadataApi
                    -> CollectionName
                    -> Component
                    -> Component
                    -> Logger
                    -> IO (Either [ByteString] Component)
mergeComponentFiles env wfw metadataApi collectionName x y logger = do

    let cn = getCollectionPath env collectionName
    createDirectoryIfMissing True cn
    cmpName <- U.toString <$> U.nextRandom
    dest <- canonicalizePath $ concat [cn, "/", cmpName]

    let params = [ "merge"
                 , dest
                 , path x
                 , path y ]

    let bin = Bin { getCmd   = indexerBinary env
                  , getArgs  = params
                  , getInput = Nothing }

    runBs bin >>= \case

        Left l ->
            pure $ Left l

        Right (stderr, "") -> do

            interleaveWarcFiles wfw x y dest -- TODO catch

            mergeMetadata metadataApi (path x) (path y) dest -- TODO catch

            Right <$> createComponent (numDocs x + numDocs y) dest
