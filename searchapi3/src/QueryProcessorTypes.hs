{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings #-}

module QueryProcessorTypes where

import           Control.DeepSeq            (NFData)
import           Control.Lens
import           Data.Aeson                 (FromJSON, ToJSON (..))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict as M
import           Data.Set                   (Set)
import qualified Data.Set as S
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
                , metadata   :: !(Maybe (Map Text Text))
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
                , metadata   = mempty
                }

exampleQueryResult2 :: QueryResult
exampleQueryResult2 =
    QueryResult { uri        = "http://www.baz.com"
                , score      = 0.5
                , term_count = 3
                , metadata   = mempty
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

newtype SpellingSuggestions =
    SpellingSuggestions { unSpellingSuggestions :: Map Text (Map Int (Set Text)) }
        deriving Generic

instance ToJSON SpellingSuggestions where
    toJSON (SpellingSuggestions m) = toJSON m

instance NFData SpellingSuggestions

instance Semigroup SpellingSuggestions where
    SpellingSuggestions m1 <> SpellingSuggestions m2 =
        SpellingSuggestions $ M.unionWith (M.unionWith S.union) m1 m2

instance Monoid SpellingSuggestions where
    mempty = SpellingSuggestions mempty

instance ToSchema SpellingSuggestions where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Spelling suggestions"
        & mapped.schema.example     ?~ toJSON exampleSpellingSuggestions

exampleSpellingSuggestions :: SpellingSuggestions
exampleSpellingSuggestions =
    SpellingSuggestions $ M.singleton "foo" (M.singleton 1 (S.singleton "food"))