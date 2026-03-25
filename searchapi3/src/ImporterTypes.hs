{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric #-}

module ImporterTypes where

import Data.Aeson      (FromJSON)
import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

newtype NumDocsReply =
    NumDocsReply { num_docs :: Int }
      deriving (Generic, FromJSON)

instance NFData NumDocsReply
