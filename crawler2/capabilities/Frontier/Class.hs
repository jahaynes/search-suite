module Frontier.Class where

import Restful.Types (Url)

newtype Millis =
    Millis Int deriving (Eq, Ord)

data NextUrl
    = NextUrl !Url
    | RetryIn !Millis
    | NoMoreUrls

class Frontier m where

    completed :: Url -> m ()

    insert :: Url -> m ()

    nextUrl :: m NextUrl
