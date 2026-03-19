{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Extensions.WarcIndexer ( WarcIndexer (..)
                              , createWarcIndexer
                              ) where

import Api                 (Doc (..), IndexRequest (..))
import Data.Warc.Body
import Data.Warc.Header
import Data.Warc.Key
import Data.Warc.Value
import Data.Warc.WarcEntry (WarcEntry (..), decompress)
import Logger              (Logger (..))
import Indexer             (Indexer (..))
import Types
import WarcFileReader      (WarcFileReader (..))

import           Control.Monad
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Text.Encoding          (decodeUtf8')
import           Data.Vector                 (Vector)
import qualified Data.Vector as V

newtype WarcIndexer =
    WarcIndexer { indexLocalWarcFile :: CollectionName
                                     -> FilePath
                                     -> IO (Either String ()) }

createWarcIndexer :: WarcFileReader
                  -> Indexer
                  -> Logger
                  -> WarcIndexer
createWarcIndexer warcFileReader indexer logger =
    WarcIndexer { indexLocalWarcFile = indexLocalWarcFileImpl warcFileReader indexer logger }

-- TODO unnecessary encoding/decoding?
indexLocalWarcFileImpl :: WarcFileReader
                       -> Indexer
                       -> Logger
                       -> CollectionName
                       -> FilePath
                       -> IO (Either String ())
indexLocalWarcFileImpl warcFileReader indexer logger collectionName warcFile =

    batchedRead warcFileReader
                warcFile
                newIndexify

    where
    newIndexify :: Vector WarcEntry -> IO ()
    newIndexify ds = do

        let (lefts, rights) = V.partitionWith id
                            . V.map toDoc
                            $ ds

        unless (null lefts)
               (error "foo")

        -- TODO: Easy batching win to be had here
        V.mapM_ (\d -> indexDocs indexer collectionName (IndexRequest [d])) rights

        where
        toDoc :: WarcEntry -> Either ByteString Doc
        toDoc we = do

            let (WarcEntry header (UncompressedBody body)) = decompress we

            uri <- case getValue (OptionalKey WarcTargetURI) header of
                       Nothing                -> Left "No URI"
                       Just (StringValue uri) -> Right uri

            case getValue (MandatoryKey WarcType) header of
                Just (StringValue "response") -> Right ()
                _                             -> Left "Not a WARC response"

            uri'  <- case decodeUtf8' uri of
                         Left e  -> Left $ "Invalid URI: " <> uri
                         Right x -> Right x
            body' <- case decodeUtf8' body of
                         Left e  -> Left $ "Invalid body: " <> C8.take 150 body
                         Right x -> Right x

            pure $ Doc uri' body'
