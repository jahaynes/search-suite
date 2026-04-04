{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             QuasiQuotes #-}

module Extensions.GitIndexer ( GitIndexer(..)
                             , createGitIndexer
                             ) where

import Bin     (Bin (..), runBs)
import Indexer (Indexer (indexLocalFiles))
import Types   (CollectionName)

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.String.Interpolate          (i)
import           Data.Text                        (Text)
import           System.Directory                 (canonicalizePath, doesDirectoryExist)

newtype GitIndexer =
    GitIndexer { indexGitPath :: CollectionName -> FilePath -> IO (Either [Text] Int) }

createGitIndexer :: Monad m => Indexer -> m GitIndexer
createGitIndexer indexer = pure GitIndexer { indexGitPath = indexGitPathImpl indexer }

-- TODO index 'local!' git repository
-- TODO fix dupes problem
indexGitPathImpl :: Indexer -> CollectionName -> FilePath -> IO (Either [Text] Int)
indexGitPathImpl indexer collection fp =

    doesDirectoryExist fp >>= \case

        False ->

            pure $ Left ["Git path doesn't exist!"]

        True -> do

            cfp <- canonicalizePath fp

            let bin =
                    Bin { getCmd = "git"
                        , getArgs = [ [i|--git-dir=#{cfp}/.git|]
                                    , "ls-files" ]
                        , getInput = Nothing }

            runBs bin >>= \case

                Left l ->
                    pure (Left l)

                Right (stderr, stdout) ->

                    let fs = map (\s -> [i|#{cfp}/#{L8.unpack s}|])
                           . L8.lines
                           $ stdout

                    in indexLocalFiles indexer collection fs
