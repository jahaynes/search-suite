module Metrics where

import Data.Time.Clock

timeMetric :: String -> IO a -> IO a
timeMetric msg a = do
    start <- getCurrentTime
    y     <- a
    end   <- getCurrentTime
    -- putStrLn $ show (diffUTCTime end start) ++ " " ++ msg
    pure y