module CompactorStrategy ( fibSet, hybridStrategy ) where

import           Component     ( Component )
import           Types         ( numDocs )

import           Data.IntSet                 (IntSet)
import qualified Data.IntSet           as IS
import           Data.List                   (maximumBy, sortBy)
import           Data.Ord                    (comparing)
import           Data.Set                    (Set)
import qualified Data.Set              as S

fibSet :: IntSet
fibSet = IS.fromAscList $! take 100 (tail fibs)
    where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

hybridStrategy :: Set Component
               -> Maybe (String, Component, Component)
hybridStrategy componentSet =
    let components = sortBy (comparing numDocs) $ S.toList componentSet
        nonFibComponents = filter (not . isFib) components
    in if null nonFibComponents
        then largestFibonacciStrategy' components
        else nonFibonacciStrategy components nonFibComponents

    where
    isFib :: Component -> Bool
    isFib cmp = numDocs cmp `IS.member` fibSet

nonFibonacciStrategy :: [Component]
                     -> [Component]
                     -> (Maybe ([Char], Component, Component))
nonFibonacciStrategy components nonFibComponents =
    -- Find gap
            let maxNonFib    = maximum nonFibComponents
                Just nextFib = IS.lookupGT (numDocs maxNonFib) fibSet
                gap          = nextFib - numDocs maxNonFib
                fillers      = filter (\c -> numDocs c == gap)
                             $ filter (\c -> c /= maxNonFib) components

            -- Can we fill that gap with one component?
            in case fillers of

                -- Yes
                (f:_) -> Just ("gap 1", f, maxNonFib)

                -- No
                _ -> incrementalGapFill components maxNonFib gap

incrementalGapFill :: [Component]
                   -> Component
                   -> Int
                   -> (Maybe (String, Component, Component))
incrementalGapFill components maxNonFib gap =
    let otherComponents = filter (\c -> c /= maxNonFib) components
    -- Algorithm choice: find the best adjacent (O(n)) pair, or best non-adjacent (O(n2)) pair?
    -- Trying adjacent pairs for now
        candidatePairs = filter (\(a,b) -> numDocs a + numDocs b <= gap)
                       $ pairs otherComponents
    in if null candidatePairs
           then Nothing
           else let (p, q) = maximumBy (comparing (\(a,b) -> numDocs a + numDocs b)) candidatePairs
                in Just ("incrementalGapFill", p, q)

largestFibonacciStrategy' :: [Component] -> (Maybe (String, Component, Component))
largestFibonacciStrategy' components = do
    let candidates = map (\(a,b) -> [a,b])
                   . sortBy (flip $ comparing (\(a, b) -> numDocs a + numDocs b))
                   . filter (\(a, b) -> (numDocs b + numDocs a) `IS.member` fibSet)
                   $ pairs components  -- groupby was wrong here.
    case candidates of
        ([x,y]:_) -> Just ("fib", x, y)
        _         -> Nothing

pairs :: [a] -> [(a, a)]
pairs     [] = []
pairs (x:xs) = go [] x xs
    where
    go acc _     [] = reverse acc
    go acc p (y:ys) = go ((p,y):acc) y ys
