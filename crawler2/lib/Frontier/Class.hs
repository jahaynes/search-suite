module Frontier.Class where

import Restful.Types (Url)
import Time.Class    (Millis)

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
