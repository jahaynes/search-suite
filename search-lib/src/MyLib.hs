{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyLib (someFunc) where

import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Except     (ExceptT, runExceptT)
import           Control.Monad.Trans.Reader     (ReaderT (..), ask)

data Env = Env

newtype Search a =
    Search { unSearch :: ExceptT [String] (ReaderT Env IO) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
