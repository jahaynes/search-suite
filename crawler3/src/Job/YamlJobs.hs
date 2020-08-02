{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             LambdaCase #-}

module Job.YamlJobs where

import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import           Data.List.Split              (splitOn)
import           Data.Yaml
import           GHC.Generics                 (Generic)

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
        Right jobs          -> pure jobs

chop :: String -> String
chop = concat . drop 1 . splitOn "_"
