{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Controllers.Controller ( runController ) where

import Compactor               (Compactor)
import Controllers.Collections 
import Controllers.Diagnostic
import Controllers.Indexation
import Controllers.Query
import Environment             (Environment)
import Errors.Errors           (Error)
import Indexer                 (Indexer)
import Network.Fetcher         (Fetcher)
import Query.QueryProcessor    (QueryProcessor)
import Registry                (Registry)

import Control.Lens                      ((&), (.~))
import Control.Monad.IO.Class            (liftIO)
import Control.Monad.Trans.Except        (ExceptT)
import Data.ByteString.Char8             (ByteString)
import Data.Swagger                      (Swagger, info, title, version)
import Network.Wai.Handler.Warp          (run)
import Network.Wai.Middleware.Cors       (simpleCors)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Servant
import Servant.Swagger                   (toSwagger)
import Servant.Swagger.UI                (SwaggerSchemaUI, swaggerSchemaUIServerT)

type SearchApi = CollectionsApi
            :<|> QueryApi
            :<|> IndexationApi
            :<|> DiagnosticApi

type SearchApiWithDoc = SwaggerSchemaUI "swagger-ui" "swagger.json"
                   :<|> SearchApi

searchApiSwagger :: Swagger
searchApiSwagger = toSwagger searchApi
                 & info.title   .~ "Search Suite API"
                 & info.version .~ "3.0"

    where
    searchApi :: Proxy SearchApi
    searchApi = Proxy

runController :: Compactor
              -> Environment
              -> Indexer
              -> Fetcher (ExceptT Error IO)
              -> QueryProcessor
              -> Registry
              -> (ByteString -> IO ())
              -> IO ()
runController compactor env indexer fetcher qp registry _logger =

    run 8081 . simpleCors
             . prometheus def 
             . serve searchApiWithDoc 
             . hoistServer searchApiWithDoc liftIO 
             $ swaggerSchemaUIServerT searchApiSwagger
            :<|> collectionsServer compactor env registry
            :<|> queryServer qp
            :<|> indexationServer fetcher indexer
            :<|> diagnosticServer registry
    where
    searchApiWithDoc :: Proxy SearchApiWithDoc
    searchApiWithDoc = Proxy
