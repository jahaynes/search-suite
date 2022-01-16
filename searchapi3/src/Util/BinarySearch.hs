module Util.BinarySearch where

-- TODO use worker/wrapper
binarySearchM :: Ord a => Int
                       -> Int
                       -> a
                       -> (Int -> IO (a, b))
                       -> IO (Maybe (a, b))
binarySearchM lo hi target predM = do --TODO TEST boundaries!
    let i = lo + (div (hi - lo) 2)
    (probe, dat) <- predM i
    if probe == target
        then pure $ Just (probe, dat)
        else if i == hi || i == lo
            then pure Nothing
            else if probe < target
                then binarySearchM i hi target predM
                else binarySearchM lo i target predM