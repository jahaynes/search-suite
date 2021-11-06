{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase #-}

module Job.YamlJobs where

import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import           Data.List.Split              (splitOn)
import           Data.Yaml
import           GHC.Generics                 (Generic)
import           System.Directory             (removeFile)
import           System.IO.Error              (isDoesNotExistError)
{- TODO add:
    Time between successive host hits
    Storage: mem / sqlite
    Num threads
-}

newtype Jobs =
    Jobs { jobs :: [Job]
         } deriving (Generic, FromJSON)

data Job =
    Job { j_actions  :: ![Action]
        , j_seedUrls :: ![String]
        } deriving Generic

instance FromJSON Job where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = chop
                                                , unwrapUnaryRecords = True
                                                }
data Action =
    Action { action :: !String
           , param  :: !String
           } deriving (Generic, FromJSON)

readJobs :: FilePath -> IO Jobs
readJobs filePath =
    (decodeEither' <$> C8.readFile filePath) >>= \case
        Left parseException -> error $ show parseException
        Right js            -> pure js

chop :: String -> String
chop = concat . drop 1 . splitOn "_"

initialise :: Job -> IO ()
initialise (Job actions _) = mapM_ initialiseAction actions
    where
    initialiseAction (Action "writeWarc" warcFile) =
        catchIO (removeFile warcFile)
                (\ex -> if isDoesNotExistError ex
                            then pure ()
                            else error $ show ex)
    initialiseAction                               _ = pure ()