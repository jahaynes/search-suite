{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Controllers.Query where

import QueryParams             (QueryParams (QueryParams))
import QueryProcessor          (QueryProcessor (runQuery))
import QueryProcessorTypes     (QueryResults)
import Types                   (CollectionName)

import Data.Text                         (Text)
import Data.Text.Encoding                (encodeUtf8)
import Servant

type QueryApi = "query" :> Capture "col" CollectionName
                        :> QueryParam' '[Required] "q" Text
                        :> QueryParam "n" Int
                        :> Get '[JSON] (Either String QueryResults)

queryServer :: QueryProcessor
            -> ServerT QueryApi IO
queryServer qp cn q mn =
    runQuery qp cn (QueryParams (encodeUtf8 q) mn)
