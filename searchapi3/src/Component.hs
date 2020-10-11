module Component ( Component
                 , createComponent ) where

import Types

import Data.Hashable    (Hashable, hashWithSalt)
import System.Directory (canonicalizePath)

data Component =
    Component { cmp_size     :: !Int
              , cmp_filePath :: !FilePath
              } deriving (Ord, Show)

instance HasPath Component where
    path = cmp_filePath

instance NumDocs Component where
    numDocs = cmp_size

-- Ignores sz for equality
instance Eq Component where
    Component _aSz aFp == Component _bSz bFp =
        aFp == bFp

-- Ignores sz for hashing
instance Hashable Component where
    hashWithSalt salt (Component _sz fp) =
        hashWithSalt salt fp

createComponent :: Int -> FilePath -> IO Component
createComponent sz fp =
    Component <$> pure sz
              <*> canonicalizePath fp 