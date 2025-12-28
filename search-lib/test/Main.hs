module Main ( main ) where

import Search (Env (..), Search, runSearch, throw)

main :: IO ()
main = do

    putStrLn "correctResult"
    print =<< runSearch correctResult Env

    putStrLn "immediatelyBad"
    print =<< runSearch exceptional Env

correctResult :: Search [Int]
correctResult = pure [1, 2, 3]

exceptional :: Search [Int]
exceptional = throw ["foo"]
