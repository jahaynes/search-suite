{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings #-}

module Query.QueryProcessorTypes ( SpellingSuggestions (..), QueryResults (..), QueryResult (..), UnscoredResults (..) ) where

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
import           Data.Word                  (Word32)
import           GHC.Generics               (Generic)

newtype DocId =
    DocId Word32
        deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)

instance NFData DocId

instance ToSchema DocId where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "DocId"
        & mapped.schema.example     ?~ toJSON exampleDocId

data UnscoredResult =
    UnscoredResult { ur_doc_id :: !DocId
                   , ur_uri    :: !Text
                   } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance NFData UnscoredResult

instance ToSchema UnscoredResult where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Unscored query result"
        & mapped.schema.example     ?~ toJSON exampleUnscored

data UnscoredResults =
    UnscoredResults { num_unscored     :: !Int
                    , unscored_results :: !(Set UnscoredResult)
                    } deriving (Show, Generic, FromJSON, ToJSON)

instance Semigroup UnscoredResults where
    UnscoredResults _ ds1 <> UnscoredResults _ ds2 =
        let ds = ds1 <> ds2 in
        UnscoredResults (S.size ds) ds

instance Monoid UnscoredResults where
    mempty = UnscoredResults 0 mempty

instance NFData UnscoredResults

instance ToSchema UnscoredResults where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Unscored query results"
        & mapped.schema.example     ?~ toJSON exampleUnscoredResults

exampleDocId :: DocId
exampleDocId = DocId 32045

exampleUnscored :: UnscoredResult
exampleUnscored = UnscoredResult { ur_doc_id = exampleDocId
                                 , ur_uri    = "https://www.foo.bar"
                                 }

exampleUnscoredResults :: UnscoredResults
exampleUnscoredResults =
    UnscoredResults { num_unscored     = 1
                    , unscored_results = S.singleton exampleUnscored
                    }

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
