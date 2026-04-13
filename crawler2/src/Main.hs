module Main (main) where

import Fetcher (fetch)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    fetch
