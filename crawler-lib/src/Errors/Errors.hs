{-# LANGUAGE OverloadedStrings #-}

module Errors.Errors where

import Url (Url, valText)

import Control.Exception.Safe (SomeException)
import Data.Aeson
import Data.Text              (Text)

data Error = FetchError { fe_url           :: !Url 
                        , fe_simpleMessage :: !String
                        , fe_statusCode    :: !(Maybe Int)
                        , fe_exception     :: !(Maybe SomeException)
                        }

           | PostError { pe_simpleMessage :: !String
                       , pe_statusCode    :: !(Maybe Int)
                       , pe_exception     :: !(Maybe SomeException)
                       }

           | EncodingError { ee_url           :: !Url
                           , ee_simpleMessage :: !String
                           }

               deriving Show

instance ToJSON Error where

    toJSON (FetchError url msg code ex) =
        object [ "type"          .= ("FetchError" :: Text)
               , "url"           .= valText url
               , "simpleMessage" .= msg
               , "statusCode"    .= toJSON code
               , "exception"     .= show ex
               ]

    toJSON (PostError msg code ex) =
        object [ "type"          .= ("PostError" :: Text)
               , "simpleMessage" .= msg
               , "statusCode"    .= toJSON code
               , "exception"     .= show ex
               ]

    toJSON (EncodingError url msg) =
        object [ "type"          .= ("EncodingError" :: Text)
               , "url"           .= valText url
               , "simpleMessage" .= msg
               ]
