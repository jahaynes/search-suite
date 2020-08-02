module Component ( Component
                 , createComponent ) where

import Types

import System.Directory (canonicalizePath)

data Component =
    Component { cmp_size     :: !Int
              , cmp_filePath :: !FilePath
              } deriving (Eq, Ord, Show)

createComponent :: Int -> FilePath -> IO Component
createComponent sz fp =
    Component <$> pure sz
              <*> canonicalizePath fp 

instance HasPath Component where
    path = cmp_filePath

instance NumDocs Component where
    numDocs = cmp_size
