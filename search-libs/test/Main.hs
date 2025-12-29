{-# LANGUAGE QuasiQuotes #-}

module Main ( main ) where

import Search (Env (..), Search, runSearch, throw)

import qualified Data.Text.IO as T

import Data.String.Interpolate 

main :: IO ()
main = do

    pure ()
    {-
    T.putStrLn [i|foo|]

    putStrLn "correctResult"
    print =<< runSearch correctResult Env

    putStrLn "immediatelyBad"
    print =<< runSearch exceptional Env

correctResult :: Search [Int]
correctResult = pure [1, 2, 3]

exceptional :: Search [Int]
exceptional = throw ["foo"]
-}