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

import Registry            (Registry (..))
import Types
import WarcFileReader      (WarcFileReader (..))
import WarcFileWriter      (WarcFileWriter (..))

import           Control.Exception.Safe           (catchIO)
import           Control.Monad                    (void)
import           Data.Aeson                       (encode)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List                        (sort)
import           Data.Text.Encoding
import           Data.Vector                      (Vector)
import qualified Data.Vector                as V
import           Debug.Trace                      (trace)
import           System.Process                   (readProcessWithExitCode)
import           System.IO.Temp                   (getCanonicalTemporaryDirectory, createTempDirectory)

data Indexer =
    Indexer { indexDocuments      :: CollectionName -> [Doc] -> IO (Either String Int)
            , indexLocalWarcFile  :: CollectionName -> FilePath -> IO (Either ByteString ())
            }

createIndexer :: Environment
              -> WarcFileReader
              -> WarcFileWriter
              -> Compactor
              -> Registry
              -> Indexer
createIndexer env wfr writer cpc reg =
    Indexer { indexDocuments      = indexDocumentsImpl env writer cpc reg
            , indexLocalWarcFile  = newLocalWarcFileIndex env wfr writer cpc reg
            }

-- TODO exceptions
indexDocumentsImpl :: Environment
                   -> WarcFileWriter
                   -> Compactor
                   -> Registry
                   -> CollectionName
                   -> [Doc]
                   -> IO (Either String Int)
indexDocumentsImpl env writer compactor registry collectionName unsortedDocs = do

    let ds = sort unsortedDocs

    let numDocs = length ds

    if numDocs == 0

        then pure $ Right 0

        else do

            -- Create temp dir
            idxCmpDir <- getCanonicalTemporaryDirectory >>= (`createTempDirectory` "idx-cmp")

            -- Run indexer in tmp directory
            let bin   = indexerBinary env
                args  = ["index_json", idxCmpDir]
                stdin = encode (IndexRequest ds)

            eOut <- catchIO (Right <$> readProcessWithExitCode bin args (L8.unpack stdin))
                            (\ex -> pure . Left . show $ ex)

            case eOut of 

                Left e -> pure . Left . show $ e

                Right (code, stdout, stderr) -> do

                    print code

                    -- Also write out the warc file and offsets       
                    let destWarcFile = idxCmpDir <> "/" <> "file.warc"
                        destOffsets  = idxCmpDir <> "/" <> "file.offs"
                    writeWarcFile writer destWarcFile destOffsets ds

                    -- Import the tmp index into collection
                    component <- createComponent numDocs idxCmpDir
                    registerFromTmp registry collectionName component

                    -- Run the compactor
                    void $ compact compactor collectionName

                    pure $ Right numDocs

-- TODO unnecessary encoding/decoding?
newLocalWarcFileIndex :: Environment
                      -> WarcFileReader
                      -> WarcFileWriter
                      -> Compactor
                      -> Registry
                      -> CollectionName
                      -> FilePath
                      -> IO (Either ByteString ())
newLocalWarcFileIndex env warcFileReader writer compactor registry collectionName warcFile =

    batchedRead warcFileReader
                warcFile
                newIndexify

    where
    newIndexify :: Vector WarcEntry -> IO ()
    newIndexify docs = do

        let docs' = V.mapMaybe toDoc docs

        V.mapM_ (\d -> indexDocumentsImpl env writer compactor registry collectionName [d]) docs' 

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
