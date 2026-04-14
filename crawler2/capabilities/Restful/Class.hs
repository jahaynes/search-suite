module Restful.Class ( Restful (..) ) where

import Restful.Types

import Data.Text                 (Text)

class Restful m where

    fetchGet :: String -> m (Either [Text] Response)
