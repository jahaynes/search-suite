{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types ( CollectionName (..)
             , getCollectionPath
             , parseCollectionName
             , HasPath (..)
             , Logger (..)
             , NumDocs (..)
             , Threadedness (..)
             ) where

import Environment (Environment (..))

import Control.Monad.Fail (fail)
import Data.Aeson         (ToJSON)
import Data.Char          (isAlphaNum)
import Data.Hashable      (Hashable, hashWithSalt)

import Servant.API (FromHttpApiData)
import Prelude hiding     (fail)

class HasPath a where
    path :: a -> FilePath

class NumDocs a where
    numDocs :: a -> Int

newtype CollectionName =
    CollectionName String
        deriving (Eq, Ord, Show, ToJSON, FromHttpApiData)

instance Hashable CollectionName where
    hashWithSalt salt (CollectionName cn) = hashWithSalt salt cn

getCollectionPath :: Environment -> CollectionName -> FilePath
getCollectionPath env (CollectionName name) = collectionsDir env <> "/" <> name

-- TODO ensure this is used via FromHttpApiData
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

data Threadedness = Single
                  | Multi
