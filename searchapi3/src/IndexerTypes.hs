{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric #-}

module IndexerTypes where

import Data.Aeson   (FromJSON)
import GHC.Generics (Generic)

data IndexerReply =
    IndexerReply { num_docs  :: !Int
                 , num_terms :: !Int
                 } deriving (Generic, FromJSON)
