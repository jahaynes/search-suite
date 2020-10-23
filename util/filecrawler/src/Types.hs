{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Types where

import Data.Aeson   (ToJSON)
import Data.Text    (Text)
import GHC.Generics (Generic)

newtype Filepath =
    Filepath { value :: String
             } deriving (Show, Generic, ToJSON)

newtype IndexRequest =
    IndexRequest { docs :: [Doc] }
        deriving (Generic, ToJSON)

data Doc = 
    Doc { url     :: !Text
        , content :: !Text
        } deriving (Generic, ToJSON)
