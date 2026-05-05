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

    newFrontier :: IO m

    completed :: m -> Url -> UrlResult -> IO ()

    insert :: m -> Url -> IO ()

    nextUrl :: m -> Millis -> IO NextUrl -- Probably don't need millis in the sig
