module Exception where

import Data.Text (Text)

class Exception m where
    throw :: [Text] -> m a
