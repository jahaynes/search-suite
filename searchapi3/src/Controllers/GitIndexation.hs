{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Controllers.GitIndexation ( GitIndexationApi
                                 , gitIndexationServer
                                 ) where

import Extensions.GitIndexer (GitIndexer (..))
import Types                 (CollectionName)

import Data.Text (Text)
import Servant

type GitIndexationApi =
    "indexGit" :> Capture "col" CollectionName
               :> ReqBody '[PlainText] FilePath
               :> Post '[JSON] (Either [Text] Int)

gitIndexationServer :: GitIndexer
                    -> ServerT GitIndexationApi IO
gitIndexationServer gitIndexer =
    indexGitPath gitIndexer
