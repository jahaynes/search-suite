module Crawler.Class where

import Restful.Types (Url)

import Control.Concurrent.Async (Async)

class Crawler c where

    addUrl :: Url -> c ()

    start :: c (Async ())
