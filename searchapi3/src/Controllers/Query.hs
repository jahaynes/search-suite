{-# LANGUAGE DataKinds,
             LambdaCase,
             OverloadedStrings,
             TypeOperators #-}

module Controllers.Query (QueryApi, queryServer) where

import Component               (Component (cmp_filePath))
import Data.Warc.Body
import Data.Warc.WarcEntry
import QueryParams             (QueryParams (QueryParams))
import QueryProcessor          (QueryProcessor (..))
import QueryProcessorTypes     (SpellingSuggestions, QueryResults)
import Registry                (Registry (..))
import Types                   (CollectionName)
import WarcFileReader          (WarcFileReader (..))

import Control.Concurrent.STM  (atomically)
import Control.Monad           (forM)
import Data.Maybe              (catMaybes, fromMaybe)
import Data.Set                (toList)
import Data.Text               (Text)
import Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import Safe                    (headMay)
import Servant

type QueryApi = "query" :> Capture "col" CollectionName
                        :> QueryParam' '[Required] "q" Text
                        :> QueryParam "n" Int
                        :> Get '[JSON] QueryResults

           :<|> "spelling" :> Capture "col" CollectionName
                           :> QueryParam' '[Required] "s" Text
                           :> QueryParam "n" Int
                           :> Get '[JSON] (Either String SpellingSuggestions)

           :<|> "cached" :> Capture "col" CollectionName
                         :> QueryParam' '[Required] "url" Text
                         :> Get '[JSON] Text    -- TODO return HTML

queryServer :: QueryProcessor
            -> Registry
            -> WarcFileReader
            -> ServerT QueryApi IO
queryServer qp reg wfr = serveQuery
                    :<|> serveSpelling
                    :<|> getCached

    where
    serveQuery cn q mn =
        runQuery qp cn (QueryParams (encodeUtf8 q) mn) >>= \case
            Left e        -> error $ show e
            Right results -> pure results

    serveSpelling cn s mn =
        runSpelling qp cn s mn

    -- TODO take a read lock on each component
    getCached cn url = do

        paths <- map cmp_filePath
               . toList
             <$> atomically (viewCollectionComponents reg cn)

        mBodies <- forM paths $ \path ->
            let warcFile = path <> "/file.warc"
                warcOffs = path <> "/file.offs"
                url'     = encodeUtf8 url
            in fmap decompressedText <$> findWarcEntry wfr warcFile warcOffs url'

        pure $ fromMaybe "{not found!}"
                         (headMay $ catMaybes mBodies)

decompressedText :: WarcEntry -> Text
decompressedText entry =
    let UncompressedBody body = getBody $ decompress entry
    in decodeUtf8 body
