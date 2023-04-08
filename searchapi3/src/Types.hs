{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving,
             OverloadedStrings #-}

module Types ( CollectionName (..)
             , getCollectionPath
             , parseCollectionName
             , HasPath (..)
             , Logger (..)
             , NumDocs (..)
             ) where

import Environment (Environment (..))

import           Control.Monad.Fail (fail)
import           Data.Aeson         (ToJSON)
import           Data.Char          (isAlphaNum)
import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.Swagger       (ToParamSchema, ToSchema)
import           Data.Text          (Text)
import qualified Data.Text as T
import           GHC.Generics       (Generic)
import           Servant.API        (FromHttpApiData (..))

import           Prelude hiding     (fail)

class HasPath a where
    path :: a -> FilePath

class NumDocs a where
    numDocs :: a -> Int

newtype CollectionName =
    CollectionName String
        deriving (Eq, Ord, Show, Generic, ToJSON)

instance ToParamSchema CollectionName 

instance ToSchema CollectionName

instance FromHttpApiData CollectionName where
    parseUrlPiece   = parseCollectionName'
    parseQueryParam = parseCollectionName'

instance Hashable CollectionName where
    hashWithSalt salt (CollectionName cn) = hashWithSalt salt cn

getCollectionPath :: Environment -> CollectionName -> FilePath
getCollectionPath env (CollectionName name) = collectionsDir env <> "/" <> name

-- TODO dedupe
parseCollectionName' :: Text -> Either Text CollectionName
parseCollectionName' str
    | T.all (\c -> isAlphaNum c || c == '-') str =
        Right (CollectionName $ T.unpack str)
    | otherwise =
        Left "Only alphanumeric chars, -, allowed in collection name"

parseCollectionName :: MonadFail m => String -> m CollectionName
parseCollectionName str
    | all (\c -> isAlphaNum c || c == '-') str
        = pure (CollectionName str)
    | otherwise = fail "Only alphanumeric chars, -, allowed in collection name"

newtype Collection =
    Collection { col_filePath :: FilePath
               } deriving Show

data Logger = CompactorLogger
            | ControllerLogger
            | IndexerLogger
            | QueryProcessorLogger
            | RegistryLogger
            | WarcFileReaderLogger
                deriving Show
