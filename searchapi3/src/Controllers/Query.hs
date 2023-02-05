{-# LANGUAGE DataKinds,
             LambdaCase,
             TypeOperators #-}

module Controllers.Query where

import QueryParams             (QueryParams (QueryParams))
import QueryProcessor          (QueryProcessor (..))
import QueryProcessorTypes     (SpellingSuggestions, QueryResults)
import Types                   (CollectionName)

import Data.Text               (Text)
import Data.Text.Encoding      (encodeUtf8)
import Servant

type QueryApi = "query" :> Capture "col" CollectionName
                        :> QueryParam' '[Required] "q" Text
                        :> QueryParam "n" Int
                        :> Get '[JSON] QueryResults

           :<|> "spelling" :> Capture "col" CollectionName
                           :> QueryParam' '[Required] "s" Text
                           :> QueryParam "n" Int
                           :> Get '[JSON] (Either String SpellingSuggestions)

queryServer :: QueryProcessor
            -> ServerT QueryApi IO
queryServer qp = serveQuery :<|> serveSpelling

    where
    serveQuery cn q mn =
        runQuery qp cn (QueryParams (encodeUtf8 q) mn) >>= \case
            Left e        -> error $ show e
            Right results -> pure results

    serveSpelling cn s mn =
        runSpelling qp cn s mn