{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Indexer ( Indexer (..)
               , createIndexer
               ) where

import Api             (IndexRequest (..), Doc (..))
import Bin             (Bin (..), runBs, runCbor)
import Compactor       (Compactor (..))
import Component
import Environment     (Environment (..))
import Metadata        (MetadataApi (generateMetadata))
import Protocol.Encode (lcbor)
import Protocol.Types  (IndexReply (..), Input (..), InputDoc (..))
import Registry        (Registry (..))
import Types
import WarcFileWriter  (WarcFileWriter (..))

import           Control.Concurrent.STM           (STM, atomically)
import           Control.Monad                    (forM, void)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Either                      (partitionEithers)
import           Data.Functor                     ((<&>))
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Vector                as V
import           System.IO.Temp                   (getCanonicalTemporaryDirectory, createTempDirectory)
import           UnliftIO.Exception               (bracket)

data Indexer =
    Indexer { indexDocs       :: !(CollectionName -> IndexRequest -> IO (Either [Text] Int))
            , indexLocalFiles :: !(CollectionName -> [FilePath] -> IO (Either [Text] ()))
            , deleteDocument  :: !(CollectionName -> String -> IO (Either [Text] ()))
            , isDocDeleted    :: !(CollectionName -> String -> IO (Either [Text] (Map Text Int)))
            }

createIndexer :: Environment
              -> WarcFileWriter
              -> MetadataApi
              -> Compactor
              -> Registry
              -> Indexer
createIndexer env writer metadataApi cpc reg =
    Indexer { indexDocs       = indexDocsImpl env writer metadataApi cpc reg
            , indexLocalFiles = indexLocalFileImpl env writer metadataApi cpc reg
            , deleteDocument  = deleteDocumentImpl env reg
            , isDocDeleted    = isDocDeletedImpl env reg
            }

indexDocsImpl :: Environment
              -> WarcFileWriter
              -> MetadataApi
              -> Compactor
              -> Registry
              -> CollectionName
              -> IndexRequest
              -> IO (Either [Text] Int)
indexDocsImpl env writer metadataApi compactor registry collectionName (IndexRequest ds) = do

    idxCmpDir <- getCanonicalTemporaryDirectory >>= (`createTempDirectory` "idx-cmp")

    let input = Input
              . V.map (\(Doc u c) -> InputDoc u c "none") --TODO ugly
              $ V.fromList ds

    let bin' = Bin { getCmd   = indexerBinary env
                   , getArgs  = ["index_docs", idxCmpDir]
                   , getInput = Just (lcbor input) }

    runCbor bin' >>= \case

        Left l -> pure
                . Left
                . map TE.decodeUtf8
                $ l

        Right (stderr, IndexReply nd _ _) ->

            case fromIntegral nd of

                0 -> pure $ Right 0

                nDocs -> do

                    -- Write out the warc file and offsets
                    let destWarcFile = idxCmpDir <> "/" <> "file.warc"
                        destOffsets  = idxCmpDir <> "/" <> "file.offs"
                    writeWarcFile writer destWarcFile destOffsets ds

                    -- Extract / write out metadata
                    generateMetadata metadataApi idxCmpDir

                    -- Import the tmp index into collection
                    component <- createComponent nDocs idxCmpDir
                    registerFromTmp registry collectionName component

                    -- Run the compactor
                    void $ compact compactor collectionName

                    pure $ Right nDocs

indexLocalFileImpl :: Environment
                   -> WarcFileWriter
                   -> MetadataApi
                   -> Compactor
                   -> Registry
                   -> CollectionName
                   -> [FilePath]  -- TO Vector? or just json
                   -> IO (Either [Text] ())
indexLocalFileImpl env writer metadataApi compactor registry collectionName filePaths = do
    -- jsonify?
    -- Unnecessary packing
    -- safe-exceptions
    unsortedDocs <- forM filePaths $ \fp ->
        Doc (T.pack fp) <$> T.readFile fp

    indexDocsImpl env writer metadataApi compactor registry collectionName (IndexRequest unsortedDocs) >>= \case

        Left e ->
            pure $ Left e

        Right i -> do
            putStrLn $ "Indexed: " ++ show i ++ " documents."
            pure $ Right ()

deleteDocumentImpl :: Environment
                   -> Registry
                   -> CollectionName
                   -> String
                   -> IO (Either [Text] ())
deleteDocumentImpl env reg collectionName docUrl =

    let acquire = atomically $ acquireComponents reg collectionName

        release = releaseComponents reg

    in bracket acquire release $ \cmps ->

        mapM deleteJob cmps <&> \rs ->

            let (lefts, rights) = partitionEithers rs

            in if null lefts

                then Right ()

                else Left
                   . map TE.decodeUtf8
                   $ concat lefts

        where
        deleteJob :: Component -> IO (Either [ByteString] ())
        deleteJob cmp =
            let bin = Bin { getCmd   = indexerBinary env
                          , getArgs  = ["delete_doc", cmp_filePath cmp, docUrl]
                          , getInput = Nothing }
            in runBs bin <&> \case
                Left l -> Left l
                Right (stderr, "") -> Right ()

isDocDeletedImpl :: Environment
                 -> Registry
                 -> CollectionName
                 -> String
                 -> IO (Either [Text] (Map Text Int))
isDocDeletedImpl env reg collectionName docUrl =

    let acquire = atomically $ acquireComponents reg collectionName

        release = releaseComponents reg

    in bracket acquire release $ \cmps ->

        mapM isDeleted cmps <&> \rs ->

            let (lefts, rights) = partitionEithers rs

            in if null lefts

                then Right
                   . M.fromListWith (+)
                   . zip rights
                   $ repeat 1

                else Left
                   . map TE.decodeUtf8
                   $ concat lefts

    where
    isDeleted :: Component -> IO (Either [ByteString] Text)
    isDeleted cmp =
        let bin = Bin { getCmd   = indexerBinary env
                      , getArgs  = ["is_deleted", cmp_filePath cmp, docUrl]
                      , getInput = Nothing }
        in runBs bin <&> \case
            Left l -> Left l
            Right (stderr, stdout) -> Right . TE.decodeUtf8 . L8.toStrict $ stdout

releaseComponents :: Registry -> [Component] -> IO ()
releaseComponents reg = mapM_ (releaseLockIO reg)

acquireComponents :: Registry -> CollectionName -> STM [Component]
acquireComponents reg collectionName = do
    cmps <- S.toList <$> viewCollectionComponents reg collectionName
    mapM_ (takeLock reg) cmps
    pure cmps