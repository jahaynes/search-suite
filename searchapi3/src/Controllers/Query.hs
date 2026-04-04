{-# LANGUAGE DataKinds,
             LambdaCase,
             OverloadedStrings,
             QuasiQuotes,
             TypeOperators #-}

module Controllers.Query (QueryApi, queryServer) where

import Component                 (Component (cmp_filePath))
import Data.Warc.Body
import Data.Warc.WarcEntry
import Logger                    (Logger (..))
import Query.QueryParams         (QueryParams (QueryParams))
import Query.QueryParser
import Query.QueryProcessor      (QueryProcessor (..))
import Query.QueryProcessorTypes (QueryResults, SpellingSuggestions, UnscoredResults)
import Query.SpellingProcessor   (SpellingProcessor (..))
import Query.StructuredProcessor (StructuredProcessor (..))
import Registry                  (Registry (..))
import Types                     (CollectionName)
import WarcFileReader            (WarcFileReader (..))

import           Control.Concurrent.STM      (atomically)
import           Control.Monad               (forM)
import           Data.Functor                ((<&>))
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Set                    (toList)
import           Data.String.Interpolate     (i)
import           Data.Text                   (Text)
import qualified Data.Text as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Safe                        (headMay)
import           Servant

type QueryApi = "query" :> Capture "col" CollectionName
                        :> QueryParam' '[Required] "q" Text
                        :> QueryParam "n" Int
                        :> Get '[JSON] QueryResults

           :<|> "structured-query" :> Capture "col" CollectionName
                                   :> ReqBody '[PlainText] Text
                                   :> Post '[JSON] (Either Text UnscoredResults)

           :<|> "spelling" :> Capture "col" CollectionName
                           :> QueryParam' '[Required] "s" Text
                           :> QueryParam "n" Int
                           :> Get '[JSON] (Either Text SpellingSuggestions)

           :<|> "cached" :> Capture "col" CollectionName
                         :> QueryParam' '[Required] "url" Text
                         :> Get '[PlainText] (Headers '[Header "Content-Disposition" Text] Text)

queryServer :: QueryProcessor
            -> SpellingProcessor
            -> StructuredProcessor
            -> Registry
            -> WarcFileReader
            -> Logger
            -> ServerT QueryApi IO
queryServer qp sp struc reg wfr logger
    =    serveQuery
    :<|> structuredQuery
    :<|> serveSpelling
    :<|> getCached

    where
    serveQuery cn q mn =
        runQuery qp cn (QueryParams (encodeUtf8 q) mn) >>= \case
            Left e        -> error $ show e
            Right results -> pure results

    structuredQuery cn txt = do
        case parseQuery (encodeUtf8 txt) of
            Left e   -> pure $ Left e
            Right sq -> do
                info logger [[i|Parsed: #{sq}|]]
                runStructured struc cn sq

    serveSpelling cn s mn =
        runSpelling sp cn s mn <&> \case
            Left l  -> Left (T.unlines l)
            Right r -> Right r

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

        pure . addHeader "inline" $ fromMaybe ("{not found!}")
                                              (headMay $ catMaybes mBodies)

decompressedText :: WarcEntry -> Text
decompressedText entry =
    let UncompressedBody body = getBody $ decompress entry
    in decodeUtf8 body
