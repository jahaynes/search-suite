module Time.Class ( Millis (..)
                  , Time (..)
                  ) where

newtype Millis =
    Millis Int deriving (Eq, Ord)

instance Show Millis where

    show (Millis ms) = show ms ++ "ms"

class Time m where

    currentMillis :: m Millis

    wait :: Millis -> m ()
