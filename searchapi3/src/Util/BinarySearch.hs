module Util.BinarySearch (binarySearchM) where

binarySearchM :: (Monad m, Ord a) => Int
                                  -> a
                                  -> (Int -> m (a, b))
                                  -> m (Maybe (a, b))
binarySearchM   0      _     _ = pure Nothing
binarySearchM len target predM = binarySearchM' 0 len target predM

-- Does not check for length 0! Use above function instead.
binarySearchM' :: (Monad m, Ord a) => Int
                                   -> Int
                                   -> a
                                   -> (Int -> m (a, b))
                                   -> m (Maybe (a, b))
binarySearchM' lo hi target predM = do
    let i = lo + div (hi - lo) 2
    (probe, dat) <- predM i
    if probe == target
        then pure $ Just (probe, dat)
        else if i == hi || i == lo
            then pure Nothing
            else if probe < target
                then binarySearchM' i hi target predM
                else binarySearchM' lo i target predM
