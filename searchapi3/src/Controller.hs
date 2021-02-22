{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Controller ( runController ) where

import Api                 (IndexRequest (docs))
import Compactor           (Compactor (mergeInto))
import Indexer             (Indexer (indexDocuments, indexLocalWarcFile))
import QueryParams         (QueryParams (QueryParams))
import QueryProcessor      (QueryProcessor (runQuery))
import QueryProcessorTypes (QueryResults)
import Registry            (Registry (listCollections, totalLocksHeld, totalNumComponents))
import Types               (CollectionName)

import Control.Lens                      ((&), (.~))
import Control.Monad.IO.Class            (liftIO)
import Data.ByteString.Char8             (ByteString)
import Data.Set                          (Set)
import Data.Swagger                      (Swagger, info, title, version)
import Data.Text                         (Text)
import Data.Text.Encoding                (encodeUtf8)
import Network.Wai.Handler.Warp          (run)
import Network.Wai.Middleware.Cors       (simpleCors)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Servant
import Servant.Swagger                   (toSwagger)
import Servant.Swagger.UI                (SwaggerSchemaUI, swaggerSchemaUIServerT)

type SearchApi = "collections" :> Get '[JSON] (Set CollectionName)

            :<|> "query" :> Capture "col" CollectionName
                         :> QueryParam' '[Required] "q" Text
                         :> QueryParam "n" Int
                         :> Get '[JSON] (Either String QueryResults)

            :<|> "index" :> Capture "col" CollectionName
                         :> ReqBody '[JSON] IndexRequest
                         :> Post '[JSON] (Either String Int)

            :<|> "indexLocal" :> Capture "col" CollectionName
                              :> ReqBody '[PlainText] String
                              :> Post '[JSON] (Either String ())

            :<|> "mergeInto" :> QueryParam' '[Required] "dest" CollectionName
                             :> QueryParam' '[Required] "src" CollectionName
                             :> Post '[JSON] ()

            :<|> "diagnostic" :> "totalNumComponents" :> Get '[JSON] Int

            :<|> "diagnostic" :> "totalLocksHeld" :> Get '[JSON] Int

type SearchApiWithDoc = SwaggerSchemaUI "swagger-ui" "swagger.json"
                   :<|> SearchApi

searchApiSwagger :: Swagger
searchApiSwagger = toSwagger searchApi
            & info.title   .~ "Search Suite API"
            & info.version .~ "3.0"

    where
    searchApi :: Proxy SearchApi
    searchApi = Proxy

server :: Compactor
       -> Indexer
       -> QueryProcessor
       -> Registry
       -> (ByteString -> IO ())
       -> ServerT SearchApiWithDoc IO 
server compactor indexer qp registry logger
    = swaggerSchemaUIServerT searchApiSwagger
 :<|> listCollections registry
 :<|> (\cn q mn -> runQuery qp cn (QueryParams (encodeUtf8 q) mn))
 :<|> (\cn ir -> indexDocuments indexer cn (docs ir))
 :<|> indexLocalWarcFile indexer
 :<|> mergeInto compactor
 :<|> totalNumComponents registry
 :<|> totalLocksHeld registry

runController :: Compactor
              -> Indexer
              -> QueryProcessor
              -> Registry
              -> (ByteString -> IO ())
              -> IO ()
runController compactor indexer qp registry logger =

    run 8081 . simpleCors
             . prometheus def 
             . serve searchApiWithDoc 
             . hoistServer searchApiWithDoc liftIO 
             $ server compactor indexer qp registry logger

    where
    searchApiWithDoc :: Proxy SearchApiWithDoc
    searchApiWithDoc = Proxy
