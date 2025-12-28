module Capability.Exception where

class Exception m where
    throw :: [String] -> m a
