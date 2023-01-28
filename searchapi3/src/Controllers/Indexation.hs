{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Controllers.Indexation where

import Api                 (Doc (Doc), IndexRequest (docs))
import Errors.Errors       (Error)
import Indexer             (Indexer (..))
import Network.Fetcher     (Fetcher (fetch))
import Page.Page
import Types               (CollectionName)
import Url                 (mkUrl, valText)

import Control.Concurrent.Async          (mapConcurrently)
import Control.Monad                     (forM_)
import Control.Monad.Trans.Except        (ExceptT, runExceptT)
import Data.Aeson                        (encode)
import Data.ByteString.Lazy.Char8        (unpack)
import Data.Char                         (isSpace)
import Data.List.Split                   (chunksOf)
import Data.Map                          (Map)
import Data.Text.Encoding                (decodeUtf8')
import Servant

type IndexationApi = "indexDoc" :> Capture "col" CollectionName
                                :> ReqBody '[JSON] IndexRequest
                                :> Post '[JSON] (Either String Int)

                :<|> "indexPage" :> Capture "col" CollectionName
                                 :> ReqBody '[PlainText] String
                                 :> Post '[JSON] (Either String ())

                :<|> "indexLocal" :> Capture "col" CollectionName
                                  :> ReqBody '[PlainText] String
                                  :> Post '[JSON] (Either String ())

                :<|> "deleteDoc" :> Capture "col" CollectionName
                                 :> ReqBody '[PlainText] String
                                 :> Delete '[JSON] (Either String ())

                :<|> "isDocDeleted" :> Capture "col" CollectionName
                                    :> ReqBody '[PlainText] String
                                    :> Post '[JSON] (Either String (Map String Int))

indexationApi :: Proxy IndexationApi
indexationApi = Proxy

indexationServer :: Fetcher (ExceptT Error IO)
                 -> Indexer
                 -> ServerT IndexationApi IO 
indexationServer fetcher indexer
    = (\cn ir -> indexDocuments indexer cn (docs ir))
 :<|> fetchUrlLines fetcher indexer
 :<|> indexLocalWarcFile indexer
 :<|> deleteDocument indexer
 :<|> isDocDeleted indexer

-- TODO message
fetchUrlLines :: Fetcher (ExceptT Error IO)
              -> Indexer
              -> CollectionName
              -> String
              -> IO (Either String ())
fetchUrlLines fetcher indexer col strUrlLines = do

    let chunks = chunksOf 4 . filter (not . all isSpace) . map trim . lines $ strUrlLines

    forM_ chunks $ \chunk -> do

        results <- mapConcurrently fetchUrlLine chunk
        -- TODO log instead
        mapM_ print results

    pure $ Right ()

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
                        _ <- indexDocuments indexer col [Doc txtUrl body]
                        pure $ Right ()
