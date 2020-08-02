{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Controller ( runController ) where

import Api
import QueryParams         ( QueryParams (..), MergeParams (..), parseQueryParams, parseMergeParams )
import Service             ( Service (..) )
import Types               ( parseCollectionName )

import Data.Aeson                              (eitherDecode', encode)
import Data.ByteString.Char8                   (ByteString, pack)
import Data.ByteString.Lazy.Char8        as L8 (unpack)
import Data.CaseInsensitive                    (CI)
import Data.Binary.Builder                     (fromByteString, fromLazyByteString)
import Data.Text                         as T  (unpack)
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp                (run)
import Network.Wai.Middleware.Prometheus 
import Text.Printf                             (printf)

app3 :: Service
     -> (ByteString -> IO ())
     -> Application
app3 service logger request respond =

    case (requestMethod request, pathInfo request) of

        ("GET", []) ->
            respond $ responseOk "TODO doco"

        ("GET", ["query", strCn]) ->
            case parseCollectionName $ T.unpack strCn of
                Nothing -> error "bad name"
                Just cn -> doQuery cn (queryString request)

        ("GET", ["diagnostic", "totalNumComponents"]) -> do
            num <- totalNumComponents service
            respond . responseBuilder status200 json . fromByteString . pack . show $ num 

        ("GET", ["diagnostic", "totalLocksHeld"]) -> do
            num <- totalLocksHeld service
            respond . responseBuilder status200 json . fromByteString . pack . show $ num 

        ("POST", ["index", strCn]) -> do

            case parseCollectionName $ T.unpack strCn of

                Nothing -> respond . responseBuilder status400 json
                                   . fromLazyByteString
                                   $ "Invalid collection name"

                Just cn -> do

                    eitherDecode' <$> strictRequestBody request >>= \case

                        Left e -> respond . responseBuilder status400 json
                                          . fromLazyByteString
                                          $ "Could not parse request"

                        Right (IndexRequest docs) -> do

                            numIndexed <- indexDocuments service cn docs

                            respond . responseBuilder status200 json
                                    . fromLazyByteString
                                    . encode
                                    $ numIndexed

        ("POST", ["indexlocal", strCn]) ->

            case parseCollectionName $ T.unpack strCn of
                Nothing -> error "bad name"
                Just cn -> do
                    warcFile <- L8.unpack <$> strictRequestBody request
                    res <- indexLocalWarcFile service cn warcFile
                    respond $ case res of
                        Left er -> responseError er
                        Right _ -> responseOk "Done"

        ("POST", ["merge"]) ->
            doMerge (queryString request)

        _ ->
            respond notFound

    where
    doQuery cn qString =
        case parseQueryParams qString of
            Nothing          -> error "Could not parse parameters for query!"
            Just queryParams -> do
                logger $ "Received query: " <> query queryParams
                result <- runQuery service cn queryParams
                case result of
                    Left e  -> error $ show e
                    Right r -> respond . responseBuilder status200 json . fromLazyByteString . encode $ r

    doMerge qString =
        case parseMergeParams qString of
            Nothing          -> error "Could not parse parameters for merging!"
            Just mergeParams -> do
                let d = dest mergeParams
                    s = src mergeParams
                logger . pack $ printf "Received merge order %s <- %s" (show d) (show s)
                mergeInto service d s
                respond $ responseOk "whatever"

responseOk :: ByteString -> Response
responseOk = responseBuilder status200 html . fromByteString

responseError :: ByteString -> Response
responseError = responseBuilder status500 html . fromByteString

notFound :: Response
notFound = responseLBS status404 plain "404 - Not Found"

runController :: Service
              -> (ByteString -> IO ())
              -> IO ()
runController service logger = do
    logger "http://localhost:8081/"
    run 8081
        (prometheus def
            (app3 service logger))

html :: [(CI ByteString, ByteString)]
html = [("Content-Type", "text/html")]

json :: [(CI ByteString, ByteString)]
json = [("Content-Type", "application/json")]

plain :: [(CI ByteString, ByteString)]
plain = [("Content-Type", "text/plain")]
