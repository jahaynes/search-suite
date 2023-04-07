{-# LANGUAGE OverloadedStrings #-}

module Indexer ( Indexer (..)
               , createIndexer
               ) where

import Api                 (IndexRequest (..), Doc (..))
import Compactor           (Compactor (..))
import Component

-- TOO coupled to Warc
import Data.Warc.Body
import Data.Warc.Key
import Data.Warc.Header
import Data.Warc.Value
import Data.Warc.WarcEntry (WarcEntry (..), decompress)

import Environment         (Environment (..))
import IndexerTypes        (IndexerReply (..))
import Registry            (Registry (..))
import Metadata            (MetadataApi (generateMetadata))
import Types
import WarcFileReader      (WarcFileReader (..))
import WarcFileWriter      (WarcFileWriter (..))

import           Control.Concurrent.STM           (atomically)
import           Control.Monad                    (forM, void)
import           Data.Aeson                       (decodeStrict, encode)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Either                      (lefts, rights)
import           Data.List                        (sort)
import           Data.Map
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Text                        (Text)
import           Data.Text.Encoding
import           Data.Vector                      (Vector)
import qualified Data.Vector                as V
import           Debug.Trace                      (trace)
import           System.Exit
import           System.Process.ByteString        (readProcessWithExitCode)
import           System.IO.Temp                   (getCanonicalTemporaryDirectory, createTempDirectory)
import           UnliftIO.Exception               (catchIO, finally)

data Indexer =
    Indexer { indexDocuments     :: !(CollectionName -> [Doc] -> IO (Either String Int))
            , indexLocalWarcFile :: !(CollectionName -> FilePath -> IO (Either String ()))
            , deleteDocument     :: !(CollectionName -> String -> IO (Either String ()))
            , isDocDeleted       :: !(CollectionName -> String -> IO (Either String (Map Text Int)))
            }

createIndexer :: Environment
              -> WarcFileReader
              -> WarcFileWriter
              -> MetadataApi
              -> Compactor
              -> Registry
              -> Indexer
createIndexer env wfr writer metadataApi cpc reg =
    Indexer { indexDocuments     = indexDocumentsImpl env writer metadataApi cpc reg
            , indexLocalWarcFile = newLocalWarcFileIndex env wfr writer metadataApi cpc reg
            , deleteDocument     = deleteDocumentImpl env reg
            , isDocDeleted       = isDocDeletedImpl env reg
            }

-- TODO exceptions
indexDocumentsImpl :: Environment
                   -> WarcFileWriter
                   -> MetadataApi
                   -> Compactor
                   -> Registry
                   -> CollectionName
                   -> [Doc]
                   -> IO (Either String Int)
indexDocumentsImpl env writer metadataApi compactor registry collectionName unsortedDocs = do

    let ds = sort unsortedDocs

    let nDocs = length ds

    if nDocs == 0

        then pure $ Right 0

        else do

            -- Create temp dir
            idxCmpDir <- getCanonicalTemporaryDirectory >>= (`createTempDirectory` "idx-cmp")

            -- Run indexer in tmp directory
            let bin   = indexerBinary env
                args  = ["index_json", idxCmpDir]
                stdin = L8.toStrict . encode $ IndexRequest ds

            eOut <- catchIO (Right <$> readProcessWithExitCode bin args stdin)
                            (\ex -> pure . Left . show $ ex)

            case eOut of 

                Left e -> pure . Left . show $ e

                Right (ExitSuccess, stdout, stderr) -> do

                    C8.putStrLn stderr

                    case decodeStrict stdout of
                        Nothing -> error "bad output"
                        Just (IndexerReply docs' terms)
                            | docs' == 0 || terms == 0 -> pure . Left $ "No docs or terms: " ++ show ds
                            | otherwise -> do

                                -- Also write out the warc file and offsets       
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

                Right (_, stdout, stderr) -> pure . Left . show $ (stdout, stderr)

-- TODO unnecessary encoding/decoding?
newLocalWarcFileIndex :: Environment
                      -> WarcFileReader
                      -> WarcFileWriter
                      -> MetadataApi
                      -> Compactor
                      -> Registry
                      -> CollectionName
                      -> FilePath
                      -> IO (Either String ())
newLocalWarcFileIndex env warcFileReader writer metadataApi compactor registry collectionName warcFile =

    batchedRead warcFileReader
                warcFile
                newIndexify

    where
    newIndexify :: Vector WarcEntry -> IO ()
    newIndexify ds = do

        let ds' = V.mapMaybe toDoc ds

        V.mapM_ (\d -> indexDocumentsImpl env writer metadataApi compactor registry collectionName [d]) ds' 

        where
        toDoc :: WarcEntry -> Maybe Doc
        toDoc we = do
            let (WarcEntry header (UncompressedBody body)) = decompress we

            StringValue uri <- getValue (OptionalKey WarcTargetURI) header
            
            case getValue (MandatoryKey WarcType) header of
                Just (StringValue "response") -> pure ()
                _                             -> Nothing

            uri'  <- case decodeUtf8' uri of
                         Left e -> trace (show (e, uri)) Nothing
                         Right x -> Just x
            body' <- case decodeUtf8' body of
                         Left e -> trace (show (e, uri)) Nothing
                         Right x -> Just x

            pure $ Doc uri' body'

{- Instead of a complicated locking mechanism,
   this just takes write locks one-by-one and send delete everywhere -}
deleteDocumentImpl :: Environment
                   -> Registry
                   -> CollectionName
                   -> String
                   -> IO (Either String ())
deleteDocumentImpl env reg collectionName docUrl = do
    let bin = indexerBinary env
    components <- S.toList <$> (atomically $ viewCollectionComponents reg collectionName)
    putStrLn $ "Deleting doc: " <> docUrl
    results <- forM components $ \cmp ->
        finally (isDeletedJob bin cmp)
                (releaseLockIO reg cmp)

    mapM_ print results

    pure $ case lefts results of
               [] -> Right ()
               ls -> Left $ unlines ls

        where
        isDeletedJob :: FilePath -> Component -> IO (Either String (ExitCode, ByteString, ByteString))
        isDeletedJob bin cmp = do
            atomically $ takeLock reg cmp
            let args = ["delete_doc", cmp_filePath cmp, docUrl]
            catchIO (Right <$> readProcessWithExitCode bin args "")
                    (\ex -> pure . Left . show $ ex)

isDocDeletedImpl :: Environment
                 -> Registry
                 -> CollectionName
                 -> String
                 -> IO (Either String (Map Text Int))
isDocDeletedImpl env reg collectionName docUrl = do
    let bin = indexerBinary env
    components <- S.toList <$> (atomically $ viewCollectionComponents reg collectionName)
    results <- forM components $ \cmp ->
        finally (deleteJob bin cmp)
                (releaseLockIO reg cmp)

    pure $
        case lefts results of
            [] -> Right . M.fromListWith (+) . zip (rights results) $ repeat 1
            ls -> Left $ unlines ls

        where
        deleteJob :: FilePath -> Component -> IO (Either String Text)
        deleteJob bin cmp = do
            atomically $ takeLock reg cmp
            let args = ["is_deleted", cmp_filePath cmp, docUrl]
            r <- catchIO (Right <$> readProcessWithExitCode bin args "")
                         (\ex -> pure . Left . show $ ex)
            pure $ case r of
                       Left l -> Left l
                       Right (ExitSuccess, str, "") -> Right $ decodeUtf8 str
