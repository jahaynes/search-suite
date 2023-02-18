{-# LANGUAGE OverloadedStrings #-}

module Util.BinarySearchTests (binarySearchTests) where

import Util.BinarySearch

import           Data.Maybe                  (isJust)
import           Data.Vector                 ((!))
import qualified Data.Vector as V
import           Data.Vector.Algorithms.Heap (sort)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

binarySearchTests :: Group
binarySearchTests =
    Group "Binary search tests" [ ("finds present item", findPresentItem)
                                , ("does not find missing item", doNotFindMissingItem) ]

needleAndHay :: PropertyT IO (Char, [Char])
needleAndHay = do
    hay    <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    needle <- forAll Gen.alpha
    pure (needle, hay)

findPresentItem :: Property
findPresentItem = property $ do
    (needle, hay) <- needleAndHay
    let hayWithNeedle = V.modify sort . V.fromList $ needle:hay
    found <- binarySearchM (length hayWithNeedle) needle (\i -> pure (hayWithNeedle ! i, i))
    True === isJust found
    -- TODO verify the result

doNotFindMissingItem :: Property
doNotFindMissingItem = property $ do
    hay    <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    needle <- forAll Gen.alpha
    let hayWithoutNeedle = V.modify sort . V.fromList $ filter (/= needle) hay
    found <- binarySearchM (length hayWithoutNeedle) needle (\i -> pure (hayWithoutNeedle ! i, i))
    False === isJust found
