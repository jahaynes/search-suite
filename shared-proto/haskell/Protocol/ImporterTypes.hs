{-# LANGUAGE DeriveGeneric #-}

-- TODO un-json

module Protocol.ImporterTypes where

import Data.Aeson   (FromJSON)
import GHC.Generics (Generic)

newtype NumDocsReply =
    NumDocsReply { num_docs :: Int }
      deriving Generic

instance FromJSON NumDocsReply