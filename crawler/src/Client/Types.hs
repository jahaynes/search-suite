{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Client.Types where

import Data.Aeson
import Data.List.Split (splitOn)
import Data.Text       (Text)
import GHC.Generics    (Generic)

newtype IndexRequest =
    IndexRequest { docs :: [Doc] }
        deriving (Generic, ToJSON)

data Doc = 
    Doc { d_url     :: !Text
        , d_content :: !Text
        } deriving Generic

instance ToJSON Doc where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

chop :: String -> String
chop = concat . drop 1 . splitOn "_"
