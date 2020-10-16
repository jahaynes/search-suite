{-# LANGUAGE LambdaCase,
             OverloadedStrings #-}

module Controller ( runController ) where

import Api
import Compactor      (Compactor (..))
import Indexer        (Indexer (..))
import QueryParams    (QueryParams (..), MergeParams (..), parseQueryParams, parseMergeParams)
import QueryProcessor (QueryProcessor (..))
import Registry       (Registry (..))
import Types          (CollectionName (..), parseCollectionName)

import           Data.Aeson                        (eitherDecode', encode)
import           Data.ByteString.Char8             (ByteString, pack)
import           Data.ByteString.Lazy.Char8 as L8  (unpack)
import           Data.CaseInsensitive              (CI)
import           Data.Binary.Builder               (fromByteString, fromLazyByteString)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text as T                    (Text, unpack)
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp          (run)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Prometheus
import           Text.Printf                       (printf)

searchApiApp :: Compactor
             -> Indexer
             -> QueryProcessor
             -> Registry
             -> (ByteString -> IO ())
             -> Application
searchApiApp compactor indexer qp registry logger request respond =

    case (requestMethod request, pathInfo request) of

        ("GET", []) ->
            respond $ responseOk "TODO doco"

        ("GET", ["collections"]) ->
            listRegistryCollections

        ("GET", ["query", strCn]) ->
            case parseCollectionName $ T.unpack strCn of
                Nothing -> error "bad name"
                Just cn -> doQuery cn (queryString request)

        ("GET", ["diagnostic", "totalNumComponents"]) -> do
            num <- totalNumComponents registry
            respond . responseBuilder status200 json . fromByteString . pack . show $ num 

        ("GET", ["diagnostic", "totalLocksHeld"]) -> do
            num <- totalLocksHeld registry
            respond . responseBuilder status200 json . fromByteString . pack . show $ num 

        ("POST", ["index", strCn]) -> do

            case parseCollectionName $ T.unpack strCn of

                Nothing -> respond . responseBuilder status400 json
                                   . fromLazyByteString
                                   $ "Invalid collection name"

                Just cn ->

                    eitherDecode' <$> strictRequestBody request >>= \case

                        Left e -> respond . responseBuilder status400 json
                                          . fromByteString
                                          . pack
                                          $ "Could not parse request: " <> (take 20 $ show e) <> "..."

                        Right indexRequest -> do

                            numIndexed <- indexDocuments indexer cn (docs indexRequest)

                            respond . responseBuilder status200 json
                                    . fromLazyByteString
                                    . encode
                                    $ numIndexed

        ("POST", ["indexlocal", strCn]) ->

            case parseCollectionName $ T.unpack strCn of
                Nothing -> error "bad name"
                Just cn -> do
                    warcFile <- L8.unpack <$> strictRequestBody request
                    res <- indexLocalWarcFile indexer cn warcFile
                    respond $ case res of
                        Left er -> responseError er
                        Right _ -> responseOk "Done"

        ("POST", ["merge"]) ->
            doMerge (queryString request)

        _ ->
            respond notFound

    where
    listRegistryCollections = do
        ls <- listCollections registry
        respond . responseBuilder status200 json
                . fromLazyByteString
                . encode
                . map (\(CollectionName cn) -> M.singleton ("id"::Text) cn)
                . S.toList
                $ ls

    doQuery cn qString =
        case parseQueryParams qString of
            Nothing          -> error "Could not parse parameters for query!"
            Just queryParams -> do
                logger $ "Received query: " <> query queryParams
                result <- runQuery qp cn queryParams
                case result of
                    Left e  -> respond $ responseBuilder status500 json . fromByteString $ e
                    Right r -> respond . responseBuilder status200 json . fromLazyByteString . encode $ r

    doMerge qString =
        case parseMergeParams qString of
            Nothing          -> error "Could not parse parameters for merging!"
            Just mergeParams -> do
                let d = dest mergeParams
                    s = src mergeParams
                logger . pack $ printf "Received merge order %s <- %s" (show d) (show s)
                mergeInto compactor d s
                respond $ responseOk "whatever"

responseOk :: ByteString -> Response
responseOk = responseBuilder status200 html . fromByteString

responseError :: ByteString -> Response
responseError = responseBuilder status500 html . fromByteString

notFound :: Response
notFound = responseLBS status404 plain "404 - Not Found"

runController :: Compactor
              -> Indexer
              -> QueryProcessor
              -> Registry
              -> (ByteString -> IO ())
              -> IO ()
runController compactor indexer qp registry logger = do
    logger "http://localhost:8081/"
    run 8081
        $ simpleCors
            $ prometheus def
                $ searchApiApp compactor indexer qp registry logger

html :: [(CI ByteString, ByteString)]
html = [("Content-Type", "text/html")]

json :: [(CI ByteString, ByteString)]
json = [("Content-Type", "application/json")]

plain :: [(CI ByteString, ByteString)]
plain = [("Content-Type", "text/plain")]
