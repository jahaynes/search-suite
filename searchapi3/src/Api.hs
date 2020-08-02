{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Api where

import Data.Aeson
import Data.List.Split (splitOn)
import Data.Text       (Text)
import GHC.Generics    (Generic)

newtype IndexRequest =
    IndexRequest { docs :: [Doc] }
        deriving (Generic, ToJSON, FromJSON, Show)

data Doc = 
    Doc { d_url     :: !Text
        , d_content :: !Text
        } deriving (Generic, Eq, Ord, Show)

instance ToJSON Doc where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = chop
                                          , unwrapUnaryRecords = True
                                          }

instance FromJSON Doc where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True
                                                }

chop :: String -> String
chop = concat . drop 1 . splitOn "_"
