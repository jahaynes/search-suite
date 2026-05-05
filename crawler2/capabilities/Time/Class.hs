module Time.Class where

newtype Millis =
    Millis Int deriving (Eq, Ord)

class Time m where

    currentMillis :: m Millis
