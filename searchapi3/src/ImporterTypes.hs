{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric #-}

module ImporterTypes where

-- TODO remove

import Data.Aeson   (FromJSON)
import GHC.Generics (Generic)

newtype NumDocsReply =
    NumDocsReply { num_docs :: Int }
      deriving (Generic, FromJSON)