{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeOperators #-}

module Controllers.Controller ( runController ) where

import Compactor                  (Compactor)
import Controllers.Collections    (CollectionsApi, collectionsServer)
import Controllers.Diagnostic     (DiagnosticApi, diagnosticServer)
import Controllers.Indexation     (IndexationApi, indexationServer)
import Controllers.GitIndexation  (GitIndexationApi, gitIndexationServer)
import Controllers.Query          (QueryApi, queryServer)
import Controllers.WarcIndexation (WarcIndexationApi, warcIndexationServer)
import Environment                (Environment)
import Errors.Errors              (Error) -- TODO - used?
import Extensions.GitIndexer      (GitIndexer)
import Extensions.WarcIndexer     (WarcIndexer)
import Indexer                    (Indexer)
import Logger                     (Logger)
import Network.Fetcher            (Fetcher)
import Query.QueryProcessor       (QueryProcessor)
import Query.SpellingProcessor    (SpellingProcessor)
import Query.StructuredProcessor  (StructuredProcessor)
import Registry                   (Registry)
import WarcFileReader             (WarcFileReader (..))

import Control.Lens                      ((&), (.~))
import Control.Monad.IO.Class            (liftIO)
import Control.Monad.Trans.Except        (ExceptT)
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
            :<|> GitIndexationApi
            :<|> WarcIndexationApi
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
              -> Environment
              -> Indexer
              -> GitIndexer
              -> WarcIndexer
              -> Fetcher (ExceptT Error IO)
              -> QueryProcessor
              -> SpellingProcessor
              -> StructuredProcessor
              -> WarcFileReader
              -> Registry
              -> Logger
              -> IO ()
runController compactor env indexer gitIndexer warcIndexer fetcher qp sp struc warcFileReader registry logger =

    run 8081 . simpleCors
             . prometheus def 
             . serve searchApiWithDoc 
             . hoistServer searchApiWithDoc liftIO 
             $ swaggerSchemaUIServerT searchApiSwagger
            :<|> collectionsServer compactor env registry
            :<|> queryServer qp sp struc registry warcFileReader logger
            :<|> indexationServer fetcher indexer
            :<|> gitIndexationServer gitIndexer
            :<|> warcIndexationServer warcIndexer
            :<|> diagnosticServer registry
            :<|> serveDirectoryFileServer "frontend"

    where
    searchApiWithDoc :: Proxy SearchApiWithDoc
    searchApiWithDoc = Proxy
