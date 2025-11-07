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
import Query.QueryProcessorTypes (QueryResults, SpellingSuggestions, UnscoredResults)
import Registry                  (Registry (..))
import Types                     (CollectionName)
import WarcFileReader            (WarcFileReader (..))

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

           -- Probably not needed for FE
           :<|> "unscored-query" :> Capture "col" CollectionName
                                 :> QueryParam' '[Required] "q" Text
                                 :> Get '[JSON] UnscoredResults

           :<|> "structured-query" :> Capture "col" CollectionName
                                   :> ReqBody '[PlainText] Text
                                   :> Post '[JSON] (Either Text UnscoredResults)

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
                    :<|> serveUnscored
                    :<|> structuredQuery
                    :<|> serveSpelling
                    :<|> getCached

    where
    serveQuery cn q mn =
        runQuery qp cn (QueryParams (encodeUtf8 q) mn) >>= \case
            Left e        -> error $ show e
            Right results -> pure results

    serveUnscored cn q =
        runUnscored qp cn q >>= \case
            Left e        -> error $ show e
            Right unscored -> pure unscored

    structuredQuery cn txt =
        case parseQuery (encodeUtf8 txt) of
            Left e   -> pure $ Left e
            Right sq -> runStructured qp cn sq

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
