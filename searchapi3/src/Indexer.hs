{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Indexer ( Indexer (..)
               , createIndexer
               ) where

import Api                 (IndexRequest (..), Doc (..))
import Compactor           (Compactor (..))
import Component
import Data.Warc.Body   -- TOO coupled to Warc
import Data.Warc.Key
import Data.Warc.Header
import Data.Warc.Value
import Data.Warc.WarcEntry (WarcEntry (..), decompress)
import Environment         (Environment (..))
import Metadata            (MetadataApi (generateMetadata))
import Protocol.Encode     (lcbor, unlcbor)
import Protocol.Types      (IndexReply (..), Input (..), InputDoc (..))
import Registry            (Registry (..))
import Types
import WarcFileReader      (WarcFileReader (..))
import WarcFileWriter      (WarcFileWriter (..))

import           Control.Concurrent.STM           (atomically)
import           Control.Monad                    (forM, void)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Either                      (lefts, rights)
import           Data.Map.Strict
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Text                        (Text, pack)
import qualified Data.Text.IO as T
import           Data.Text.Encoding
import           Data.Vector                      (Vector)
import qualified Data.Vector                as V
import           Debug.Trace                      (trace)
import           System.Exit
import           System.Process.ByteString        (readProcessWithExitCode)
import qualified System.Process.ByteString.Lazy as PL
import           System.IO.Temp                   (getCanonicalTemporaryDirectory, createTempDirectory)
import           UnliftIO.Exception               (catchIO, finally)

data Indexer =
    Indexer { indexDocs          :: !(CollectionName -> IndexRequest -> IO (Either String Int))
            , indexLocalFiles    :: !(CollectionName -> [FilePath] -> IO (Either String ()))
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
    Indexer { indexDocs          = indexDocsImpl env writer metadataApi cpc reg
            , indexLocalFiles    = indexLocalFileImpl env writer metadataApi cpc reg
            , indexLocalWarcFile = indexLocalWarcFileImpl env wfr writer metadataApi cpc reg
            , deleteDocument     = deleteDocumentImpl env reg
            , isDocDeleted       = isDocDeletedImpl env reg
            }

indexDocsImpl :: Environment -> WarcFileWriter -> MetadataApi -> Compactor -> Registry -> CollectionName -> IndexRequest -> IO (Either String Int)
indexDocsImpl env writer metadataApi compactor registry collectionName (IndexRequest docs) = do
    
    idxCmpDir <- getCanonicalTemporaryDirectory >>= (`createTempDirectory` "idx-cmp")

    let bin   = indexerBinary env
        args  = ["index_docs", idxCmpDir]
        stdin = lcbor asInput

    PL.readProcessWithExitCode bin args stdin >>= \case

        -- TODO log
        (ExitSuccess, stdout, stderr) ->

            let IndexReply nd _ _ = unlcbor stdout in

            case fromIntegral nd of

                0 -> pure $ Right 0

                nDocs -> do

                    -- Write out the warc file and offsets
                    let destWarcFile = idxCmpDir <> "/" <> "file.warc"
                        destOffsets  = idxCmpDir <> "/" <> "file.offs"
                    writeWarcFile writer destWarcFile destOffsets docs

                    -- Extract / write out metadata
                    generateMetadata metadataApi idxCmpDir

                    -- Import the tmp index into collection
                    component <- createComponent nDocs idxCmpDir
                    registerFromTmp registry collectionName component

                    -- Run the compactor
                    void $ compact compactor collectionName

                    pure $ Right nDocs

        -- TODO log
        (ec, stdout, stderr) -> do
            print ec
            L8.putStrLn stdout
            L8.putStrLn stderr
            pure $ Left "Failed to index"
    where
    asInput :: Input
    asInput  = Input . V.map (\(Doc u c) -> InputDoc u c "none") --TODO ugly
                     $ V.fromList docs

indexLocalFileImpl :: Environment
                   -> WarcFileWriter
                   -> MetadataApi
                   -> Compactor
                   -> Registry
                   -> CollectionName
                   -> [FilePath]  -- TO Vector? or just json
                   -> IO (Either String ())
indexLocalFileImpl env writer metadataApi compactor registry collectionName filePaths = do
    -- jsonify?
    -- Unnecessary packing
    -- safe-exceptions
    unsortedDocs <- forM filePaths $ \fp -> do
        t <- T.readFile fp
        pure $ Doc (pack fp) t

    ei <- indexDocsImpl env writer metadataApi compactor registry collectionName (IndexRequest unsortedDocs)

    case ei of
        Left e -> pure $ Left e
        Right i -> do
            putStrLn $ "Indexed: " ++ show i ++ " documents."
            pure $ Right ()

-- TODO unnecessary encoding/decoding?
indexLocalWarcFileImpl :: Environment
                       -> WarcFileReader
                       -> WarcFileWriter
                       -> MetadataApi
                       -> Compactor
                       -> Registry
                       -> CollectionName
                       -> FilePath
                       -> IO (Either String ())
indexLocalWarcFileImpl env warcFileReader writer metadataApi compactor registry collectionName warcFile =

    batchedRead warcFileReader
                warcFile
                newIndexify

    where
    newIndexify :: Vector WarcEntry -> IO ()
    newIndexify ds = do

        let ds' = V.mapMaybe toDoc ds -- TODO report failures

        V.mapM_ (\d -> indexDocsImpl env writer metadataApi compactor registry collectionName (IndexRequest [d])) ds' 

        where
        toDoc :: WarcEntry -> Maybe Doc
        toDoc we = do
            let (WarcEntry header (UncompressedBody body)) = decompress we

            StringValue uri <- getValue (OptionalKey WarcTargetURI) header
            
            case getValue (MandatoryKey WarcType) header of
                Just (StringValue "response") -> pure ()
                _                             -> Nothing

            uri'  <- case decodeUtf8' uri of
                         Left e -> trace (show (e, uri)) Nothing    -- TODO: remove trace
                         Right x -> Just x
            body' <- case decodeUtf8' body of
                         Left e -> trace (show (e, uri)) Nothing    -- TODO: remove trace
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
