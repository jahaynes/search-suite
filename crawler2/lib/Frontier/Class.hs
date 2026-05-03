module Frontier.Class where

import Restful.Types (Url)

newtype Millis =
    Millis Int deriving (Eq, Ord)

data UrlResult = Success
               | UrlFailure !Int

data NextUrl
    = NextUrl !Url
    | RetryIn !Millis
    | NoMoreUrls

class Frontier m where

    completed :: Url -> UrlResult -> m ()

    insert :: Url -> m ()

    nextUrl :: m NextUrl
