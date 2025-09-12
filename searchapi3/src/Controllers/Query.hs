{-# LANGUAGE DataKinds,
             LambdaCase,
             OverloadedStrings,
             TypeOperators #-}

module Controllers.Query (QueryApi, queryServer) where

import Component                 (Component (cmp_filePath))
import Controllers.Mime          (Html)
import Data.Warc.Body
import Data.Warc.WarcEntry
import Query.QueryParams         (QueryParams (QueryParams))
import Query.QueryParser
import Query.QueryProcessor      (QueryProcessor (..))
import Query.QueryProcessorTypes (SpellingSuggestions, QueryResults)
import Registry                  (Registry (..))
import Types                     (CollectionName)
import WarcFileReader            (WarcFileReader (..))

import Control.Arrow             (left, right)
import Control.Concurrent.STM    (atomically)
import Control.Monad             (forM)
import Data.Maybe                (catMaybes, fromMaybe)
import Data.Set                  (toList)
import Data.Text                 (Text)
import Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import Safe                      (headMay)
import Servant

type QueryApi = "query" :> Capture "col" CollectionName
                        :> QueryParam' '[Required] "q" Text
                        :> QueryParam "n" Int
                        :> Get '[JSON] QueryResults

           :<|> "structured-query" :> ReqBody '[PlainText] Text
                                   :> Post '[JSON] (Either Text String)

           :<|> "spelling" :> Capture "col" CollectionName
                           :> QueryParam' '[Required] "s" Text
                           :> QueryParam "n" Int
                           :> Get '[JSON] (Either String SpellingSuggestions)

           :<|> "cached" :> Capture "col" CollectionName
                         :> QueryParam' '[Required] "url" Text
                         :> Get '[Html] Text

queryServer :: QueryProcessor
            -> Registry
            -> WarcFileReader
            -> ServerT QueryApi IO

queryServer qp reg wfr = serveQuery
                    :<|> previewStructuredQuery
                    :<|> serveSpelling
                    :<|> getCached

    where
    serveQuery cn q mn =
        runQuery qp cn (QueryParams (encodeUtf8 q) mn) >>= \case
            Left e        -> error $ show e
            Right results -> pure results

    previewStructuredQuery = pure . right show . left decodeUtf8 . parseQuery . encodeUtf8 -- TODO hook this up

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
