module Crawler.Class where

import Restful.Types (Url)

import Control.Concurrent.Async (Async)

class Crawler m where

    addUrl :: Url -> m ()

    start :: m ()
