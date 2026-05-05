module Multithread.Class where

class Multithread m where

    mapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)

    forConcurrently_ :: Traversable t => t a -> (a -> m ()) -> m ()