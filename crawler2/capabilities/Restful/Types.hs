{-# LANGUAGE DeriveGeneric #-}

module Restful.Types ( Response (..) ) where

import Control.DeepSeq           (NFData)
import Data.ByteString           (ByteString)
import GHC.Generics              (Generic)

data Response =
    Response { getCode :: !Int
             , getBody :: !ByteString
             } deriving Generic

instance NFData Response
