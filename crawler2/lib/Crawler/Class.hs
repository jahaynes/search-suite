module Crawler.Class ( Crawler (..) ) where

import Restful.Types (Url)

class Crawler m where

    addUrl :: Url -> m ()

    start :: m ()
