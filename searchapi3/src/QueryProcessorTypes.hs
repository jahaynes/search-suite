{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings #-}

module QueryProcessorTypes where

import           Control.DeepSeq            (NFData)
import           Control.Lens
import           Data.Aeson                 (FromJSON, ToJSON (..))
import           Data.Swagger               (ToSchema (..), defaultSchemaOptions, description, example, genericDeclareNamedSchema, schema)
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified Data.Vector           as V
import           GHC.Generics               (Generic)

data QueryResults =
    QueryResults { num_results :: !Int
                 , results     :: !(Vector QueryResult)
                 } deriving (Show, Generic, FromJSON, ToJSON, NFData)

data QueryResult =
    QueryResult { uri        :: !Text
                , score      :: !Float
                , term_count :: !Int
                , snippet    :: !(Maybe Text)   -- TODO too soon for snippet
                } deriving (Show, Generic, FromJSON, ToJSON, NFData)

instance ToSchema QueryResult where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Scored query results"
        & mapped.schema.example     ?~ toJSON exampleQueryResult1

exampleQueryResult1 :: QueryResult
exampleQueryResult1 =
    QueryResult { uri        = "http://foo.bar"
                , score      = 0.5
                , term_count = 3
                , snippet    = Just "Many lols to be had"
                }

exampleQueryResult2 :: QueryResult
exampleQueryResult2 =
    QueryResult { uri        = "http://www.baz.com"
                , score      = 0.5
                , term_count = 3
                , snippet    = Just "dozens of newspaper articles talking about AI-generated text"
                }

instance ToSchema QueryResults where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Scored query results"
        & mapped.schema.example     ?~ toJSON exampleQueryResults

exampleQueryResults :: QueryResults
exampleQueryResults =
    QueryResults { num_results = 2
                 , results = V.fromList [exampleQueryResult1, exampleQueryResult2]
                 }

limit :: Maybe Int -> QueryResults -> QueryResults
limit  Nothing qr = qr
limit (Just n) qr =
    let dropAmount = num_results qr - n
    in if dropAmount > 0
        then QueryResults { num_results = n
                          , results     = V.drop dropAmount (results qr)
                          }
        else qr
