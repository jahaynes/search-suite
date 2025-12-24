{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric #-}

module IndexerTypes where

-- TODO remove

import Data.Aeson   (FromJSON)
import GHC.Generics (Generic)

data IndexerReply =
    IndexerReply { num_docs  :: !Int
                 , num_terms :: !Int
                 , ms_taken  :: !Int
                 } deriving (Generic, FromJSON)
