{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             OverloadedStrings #-}

module Api where

import Control.Lens
import Data.Aeson
import Data.List.Split (splitOn)
import Data.Swagger    (ToSchema (..), defaultSchemaOptions, description, example, genericDeclareNamedSchema, schema)
import Data.Text       (Text)
import GHC.Generics    (Generic)

newtype IndexRequest =
    IndexRequest { docs :: [Doc] }
        deriving (Generic, ToJSON, FromJSON, Show)

instance ToSchema IndexRequest where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "A list of indexable documents"
        & mapped.schema.example     ?~ toJSON exampleDocs

data Doc = 
    Doc { d_url     :: !Text
        , d_content :: !Text
        } deriving (Generic, Eq, Ord, Show)

instance ToSchema Doc where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "An indexable document"
        & mapped.schema.example     ?~ toJSON exampleDoc1

exampleDocs :: [Doc]
exampleDocs = [exampleDoc1, exampleDoc2]

exampleDoc1 :: Doc
exampleDoc1 = Doc "http://foo.bar" "some content"

exampleDoc2 :: Doc
exampleDoc2 = Doc "http://www.baz.com" "some more content"

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
