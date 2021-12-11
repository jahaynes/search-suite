module CompactorStrategy where

import           Component     ( Component )
import           Registry      ( Registry (..) )
import           Types         ( CollectionName, numDocs )

import           Control.Concurrent.STM      (STM)
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet           as IS
import           Data.List                   (groupBy)
import qualified Data.Set              as S

fibSet :: IntSet
fibSet = IS.fromAscList $! take 100 fibs
    where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{- TODO!  need to recover from non-fibonacci cases
   e.g. when merges overlap -}
largestFibonacciStrategy :: Registry -> CollectionName -> STM (Maybe [Component])
largestFibonacciStrategy registry collectionName = do
    components <- S.toDescList <$> viewCollectionComponents registry collectionName
    let candidates = map (take 2)
                   . filter (\xs -> length xs > 1)
                   $ groupBy sumToFibonacci components
    case candidates of
        (pair@[x,y]:_) -> do takeLock registry x     -- Not very RAII FIXME
                             takeLock registry y
                             pure $ Just pair
        _              -> pure Nothing

    where
    sumToFibonacci :: Component -> Component -> Bool
    sumToFibonacci a b = (numDocs b + numDocs a) `IS.member` fibSet