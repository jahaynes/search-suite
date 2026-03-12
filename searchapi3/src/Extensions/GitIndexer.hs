{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module Extensions.GitIndexer ( GitIndexer(..)
                             , createGitIndexer
                             ) where

import Indexer (Indexer (indexLocalFiles))
import Types   (CollectionName)

import           Control.Monad (filterM)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as S
import           System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist)
import           System.Process
import           Text.Printf                          (printf)

data GitIndexer =
    GitIndexer { indexGitPath :: CollectionName -> FilePath -> IO (Either String ()) }

createGitIndexer :: Monad m => Indexer -> m GitIndexer
createGitIndexer indexer = pure GitIndexer { indexGitPath = indexGitPathImpl indexer }

-- Todo index 'local!' git repository
indexGitPathImpl :: Indexer -> CollectionName -> FilePath -> IO (Either String ())
indexGitPathImpl indexer collection fp =
    
    doesDirectoryExist fp >>= \case

        False -> pure $ Left "Git path doesn't exist!"

        True -> do

            -- TODO check the approach here.  
            -- Maybe first switch on whether the collection exists or not
            -- Maybe create index separately in temp
            -- Or just do everything in temp, and then merge_into

            -- TODO currently dupes

            cfp <- canonicalizePath fp

            (_, Just hout, _, _) <-
                createProcess (proc "git" [printf "--git-dir=%s/.git" cfp
                                        , "log"
                                        , "--pretty=format:"
                                        , "--name-only"
                                        ]){ std_out = CreatePipe }

            cxs <- map (\s -> concat [cfp, "/", C8.unpack s])
                 . S.toList
                 . S.fromList
                 . filter (not . C8.null)
                 . C8.lines
                 <$> C8.hGetContents hout

            excs <- filterM doesFileExist cxs -- is this the right way? or make sure git does it the right way?

            indexLocalFiles indexer collection excs
