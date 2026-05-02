module Restful.Class ( Restful (..) ) where

import Restful.Types (Response, Url)

import Data.Text                 (Text)

class Restful m where

    fetchGet :: Url -> m (Either [Text] Response)
