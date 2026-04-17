module Frontier.Class where

import Restful.Types (Url)

data NextUrl
    = NextUrl !Url
    | RetryInMicros !Int
    | NoMoreUrls

class Frontier m where

    insert :: Url -> m ()

    nextUrl :: m NextUrl
