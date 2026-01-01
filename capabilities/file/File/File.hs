module File where

import Control.Monad.Trans.Resource
import System.IO

-- Not mockable.  different test implementation of File elsewhere
openForReading :: FilePath -> ResourceT IO Handle
openForReading fp = do
    (_, h) <- allocate (openBinaryFile fp ReadMode) hClose
    pure h

-- Not mockable.  different test implementation of File elsewhere
openForWriting :: FilePath -> ResourceT IO Handle
openForWriting fp = do
    (_, h) <- allocate (openBinaryFile fp WriteMode) hClose
    pure h
