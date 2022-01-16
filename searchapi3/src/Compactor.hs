{-# LANGUAGE OverloadedStrings #-}

module Compactor ( Compactor (..)
                 , createCompactor
                 ) where

import           CompactorStrategy ( largestFibonacciStrategy )

import           Component         ( Component
                                   , createComponent )

import           Environment       ( Environment (indexerBinary) )

import           Registry          ( Registry (..) )

import           Snippets          ( Snippets (..) )

import           Types             ( CollectionName
                                   , getCollectionPath
                                   , numDocs
                                   , path )

import           WarcFileWriter ( WarcFileWriter (..) )

import           Control.Concurrent.STM      (atomically)
import           Control.Exception.Safe      (catchIO)  -- todo remove
import           Control.Monad               (unless)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set              as S
import qualified Data.UUID             as U
import qualified Data.UUID.V4          as U
import           System.Directory            (canonicalizePath, createDirectoryIfMissing, removeDirectoryRecursive)
import           System.Process              (callProcess)
import           UnliftIO.Exception          (bracket)
data Compactor =
    Compactor { compact   :: CollectionName -> IO Bool
              , mergeInto :: CollectionName -> CollectionName -> IO ()
              }

createCompactor :: Environment
                -> Registry
                -> WarcFileWriter
                -> Snippets
                -> (ByteString -> IO ())
                -> Compactor
createCompactor env registry wfw snippets logger =
    Compactor { compact   = compactImpl   env registry wfw snippets logger
              , mergeInto = mergeIntoImpl env registry wfw snippets logger
              }

compactImpl :: Environment
            -> Registry
            -> WarcFileWriter
            -> Snippets
            -> (ByteString -> IO ())
            -> CollectionName
            -> IO Bool
compactImpl env registry wfw snippets logger collectionName =

     bracket (atomically $ largestFibonacciStrategy registry collectionName)

             (\mLocked -> case mLocked of
                             Nothing -> pure ()
                             Just ls -> mapM_ (releaseLockIO registry) ls)

             (\mLocked -> case mLocked of

                             Nothing -> pure False

                             Just locked@[x, y] -> do

                                 logger $ "Took locks\n" <> C8.unlines (map (C8.pack . show) locked)

                                 mergeResult <- mergeComponentFiles env wfw snippets (indexerBinary env) collectionName x y logger

                                 case mergeResult of

                                     Left errMsg -> do
                                         logger errMsg
                                         pure False

                                     Right z -> do
                                         atomically $ do
                                            unregister      registry collectionName x
                                            unregister      registry collectionName y
                                            registerInPlace registry collectionName z

                                         removeDirectoryRecursive (path x)
                                         removeDirectoryRecursive (path y)
                                         pure True)

-- TODO (critical - old directory can be removed on failure)
    -- Components are left in mem?
-- TODO logging and exceptions
mergeIntoImpl :: Environment
              -> Registry
              -> WarcFileWriter
              -> Snippets
              -> (ByteString -> IO ())
              -> CollectionName
              -> CollectionName
              -> IO ()
mergeIntoImpl env reg wfw snippets logger dest src = do

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

                -- Compact
                _ <- compactImpl env reg wfw snippets logger dest

                -- Keep going
                loop

mergeComponentFiles :: Environment
                    -> WarcFileWriter
                    -> Snippets
                    -> FilePath
                    -> CollectionName
                    -> Component
                    -> Component
                    -> (ByteString -> IO ())
                    -> IO (Either ByteString Component)

mergeComponentFiles env wfw snippets indexerPath collectionName x y logger = do

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

                 interleaveWarcFiles wfw x y dest

                 mergeSnippets snippets (path x) (path y) dest

                 Right <$> createComponent (numDocs x + numDocs y) dest

        handle ioe = do
            let errMsg = "Merge failed.  Params were " ++ show mergeArgs
            logger $ C8.pack errMsg
            error errMsg

    catchIO job handle
