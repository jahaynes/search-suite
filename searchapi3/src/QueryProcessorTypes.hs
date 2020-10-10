{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric  #-}

module QueryProcessorTypes where

import           Control.DeepSeq            (NFData)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified Data.Vector           as V
import           VectorBuilder.Builder
import           VectorBuilder.Vector
import           GHC.Generics               (Generic)

data QueryResults =
    QueryResults { num_results :: !Int
                 , results     :: !(Vector QueryResult)
                 } deriving (Show, Generic, FromJSON, ToJSON, NFData)

data QueryResult =
    QueryResult { uri        :: !Text
                , score      :: !Float
                , term_count :: !Int
                , snippet    :: !(Maybe Text)
                } deriving (Show, Generic, FromJSON, ToJSON, NFData)

limit :: Maybe Int -> QueryResults -> QueryResults
limit  Nothing qr = qr
limit (Just n) qr =
    let dropAmount = num_results qr - n
    in if dropAmount > 0
        then QueryResults { num_results = n
                          , results     = V.drop dropAmount (results qr)
                          }
        else qr

instance Semigroup QueryResults where
    QueryResults n1 r1 <> QueryResults n2 r2 = QueryResults (n1 + n2) (interleaveByScore r1 r2)

-- Assumes increasing score ordering
interleaveByScore :: Vector QueryResult -> Vector QueryResult -> Vector QueryResult
interleaveByScore v1 v2 = build (builder v1 v2)
    where
    builder a b | null a = vector b
                | null b = vector a
                | otherwise =
                    let x = V.head a
                        y = V.head b
                    in if score x < score y
                          then singleton x <> builder (V.tail a) b
                          else singleton y <> builder a (V.tail b)

instance Monoid QueryResults where
    mempty = QueryResults { num_results = 0
                          , results     = mempty
                          }
