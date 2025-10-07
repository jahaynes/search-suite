{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Controllers.Query where

import Query.QueryParams         (QueryParams (QueryParams))
import Query.QueryParser
import Query.QueryProcessor      (QueryProcessor (..))
import Query.QueryProcessorTypes (SpellingSuggestions, QueryResults)
import Types                     (CollectionName)

import Control.Arrow           (left, right)
import Data.Text               (Text)
import Data.Text.Encoding      (encodeUtf8, decodeUtf8)
import Servant

type QueryApi = "query" :> Capture "col" CollectionName
                        :> QueryParam' '[Required] "q" Text
                        :> QueryParam "n" Int
                        :> Get '[JSON] (Either String QueryResults)

           :<|> "structured-query" :> ReqBody '[PlainText] Text
                                   :> Post '[JSON] (Either Text String)

           :<|> "spelling" :> Capture "col" CollectionName
                           :> QueryParam' '[Required] "s" Text
                           :> QueryParam "n" Int
                           :> Get '[JSON] (Either String SpellingSuggestions)

queryServer :: QueryProcessor
            -> ServerT QueryApi IO
queryServer qp = (\cn q mn -> runQuery qp cn (QueryParams (encodeUtf8 q) mn))
            :<|> (pure . right show . left decodeUtf8 . parseQuery . encodeUtf8) -- TODO hook this up
            :<|> (\cn s mn -> runSpelling qp cn s mn)
