module Capability.Environment where

data Env = Env

class Environment m where
    getEnv :: m Env
