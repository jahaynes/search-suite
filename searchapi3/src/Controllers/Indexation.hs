{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TupleSections,
             TypeOperators #-}

module Controllers.Indexation where

import Api                 (Doc (Doc), IndexRequest (IndexRequest))
import Errors.Errors       (Error)
import Indexer             (Indexer (..))
import Network.Fetcher     (Fetcher (fetch))
import Page.Page
import Types               (CollectionName)
import Url                 (mkUrl, valText)

import           Control.Concurrent.Async.Extra (mapConcurrentlyBounded)
import           Control.Monad.Trans.Except     (ExceptT, runExceptT)
import           Data.Aeson                     (encode)
import           Data.Bifunctor
import           Data.ByteString.Lazy.Char8     (unpack)
import           Data.Char                      (isSpace)
import           Data.Either                    (partitionEithers)
import           Data.Map                       (Map)
import qualified Data.Map.Strict as M
import           Data.Text                      (Text)
import           Data.Text.Encoding             (decodeUtf8')
import           Servant

type IndexationApi = "indexDocs" :> Capture "col" CollectionName
                                 :> ReqBody '[JSON] IndexRequest
                                 :> Post '[JSON] (Either String Int)

                :<|> "indexUrlLines" :> Capture "col" CollectionName
                                     :> ReqBody '[PlainText] String
                                     :> Post '[JSON] (Map String [String])

                :<|> "indexLocalFiles" :> Capture "col" CollectionName
                                       :> ReqBody '[PlainText] String
                                       :> Post '[JSON] (Either String ())

                :<|> "indexLocalWarcFile" :> Capture "col" CollectionName
                                          :> ReqBody '[PlainText] String
                                          :> Post '[JSON] (Either String ())

                :<|> "deleteDoc" :> Capture "col" CollectionName
                                 :> ReqBody '[PlainText] String
                                 :> Delete '[JSON] (Either String ())

                :<|> "isDocDeleted" :> Capture "col" CollectionName
                                    :> ReqBody '[PlainText] String
                                    :> Post '[JSON] (Either String (Map Text Int))

indexationApi :: Proxy IndexationApi
indexationApi = Proxy

indexationServer :: Fetcher (ExceptT Error IO)
                 -> Indexer
                 -> ServerT IndexationApi IO 
indexationServer fetcher indexer
    = indexDocs indexer
 :<|> indexUrlLines fetcher indexer
 :<|> indexLocalFilesImpl indexer
 :<|> indexLocalWarcFile indexer
 :<|> deleteDocument indexer
 :<|> isDocDeleted indexer

-- TODO move out of controller
-- TODO message
indexUrlLines :: Fetcher (ExceptT Error IO)
              -> Indexer
              -> CollectionName
              -> String
              -> IO (Map String [String])
indexUrlLines fetcher indexer col strUrlLines = do

    let maxConcurrency = 10

    let urls = filter (not . all isSpace)
             . map trim
             $ lines strUrlLines

    (fails, successes) <-   partitionEithers
                        .   zipWith (\u -> bimap (u,) (u,)) urls
                        <$> mapConcurrentlyBounded maxConcurrency fetchUrlLine urls

    pure . M.insert "success" (map fst successes)
         . M.insert "failure" (map fst fails)
         $ mempty

    where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    fetchUrlLine :: String -> IO (Either String ())
    fetchUrlLine strUrl =
        case mkUrl strUrl of
            Nothing  -> pure $ Left "Could not parse url"
            Just url -> do
                page <- runExceptT $ fetch fetcher url
                case page of
                    Left l  -> pure (Left $ "Failed to fetch page: " ++ (take 300 . unpack $ encode l))
                    Right r -> do
                        let txtUrl = valText $ p_url r
                            Right body = decodeUtf8' $ p_body r
                        _ <- indexDocs indexer col (IndexRequest [Doc txtUrl body])
                        pure $ Right ()

-- TODO use more efficient filepath - or json?
-- or dirs, or regexes
indexLocalFilesImpl :: Indexer -> CollectionName -> FilePath -> IO (Either String ())
indexLocalFilesImpl indexer cn filePaths =
    indexLocalFiles indexer cn (lines filePaths)
