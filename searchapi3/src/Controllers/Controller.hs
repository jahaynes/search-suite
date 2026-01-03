{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Controllers.Controller ( runController ) where

import Compactor               (Compactor)
import Controllers.Collections 
import Controllers.Diagnostic
import Controllers.Indexation
import Controllers.Query
import Errors.Errors           (Error)
import Indexer                 (Indexer)
import Network.Fetcher         (Fetcher)
import QueryProcessor          (QueryProcessor)
import Registry                (Registry)
import WarcFileReader          (WarcFileReader (..))

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
            :<|> Raw

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
              -> Indexer
              -> Fetcher (ExceptT Error IO)
              -> QueryProcessor
              -> WarcFileReader
              -> Registry
              -> (ByteString -> IO ())
              -> IO ()
runController compactor indexer fetcher qp warcFileReader registry _logger =

    run 8081 . simpleCors
             . prometheus def 
             . serve searchApiWithDoc 
             . hoistServer searchApiWithDoc liftIO 
             $ swaggerSchemaUIServerT searchApiSwagger
            :<|> collectionsServer compactor registry
            :<|> queryServer qp registry warcFileReader
            :<|> indexationServer fetcher indexer
            :<|> diagnosticServer registry
            :<|> serveDirectoryFileServer "frontend"

    where
    searchApiWithDoc :: Proxy SearchApiWithDoc
    searchApiWithDoc = Proxy
