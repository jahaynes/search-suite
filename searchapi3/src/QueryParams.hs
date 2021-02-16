module QueryParams where

import Types (CollectionName)

import Data.ByteString (ByteString)

data QueryParams =
    QueryParams { query      :: !ByteString
                , maxResults :: !(Maybe Int)
                }

data MergeParams =
    MergeParams { dest :: !CollectionName
                , src  :: !CollectionName
                }
