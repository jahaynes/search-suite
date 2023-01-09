module Main where

import CompactorStrategy
import Component

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.IntSet as IS

main :: IO ()
main =
    defaultMain [ bgroup "pairs" pairsBenchmark
                , bgroup "fibs" largestFibonacciStrategyBenchmark
                ]

pairsBenchmark :: [Benchmark]
pairsBenchmark =
    let xs = [1..1000] :: [Int]
    in xs `deepseq` [ bench "pairs" $ nf (sum . map snd . pairs) xs ]

largestFibonacciStrategyBenchmark :: [Benchmark]
largestFibonacciStrategyBenchmark =
    let someFibs = IS.toList fibSet
        cmps     = map (\n -> Component n ("foo/" ++ show n)) someFibs
    in cmps `deepseq` [ bench "fibs" $ nf largestFibonacciStrategy cmps ]
