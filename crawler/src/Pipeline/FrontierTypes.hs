module Pipeline.FrontierTypes where

import Url

import Data.Time.Clock (UTCTime, diffUTCTime)

newtype Now =
    Now UTCTime

data Result = Url !Url
            | WaitMicros !Int
            | Done deriving (Eq, Show)

data PushBackTime = PushBackTime
                  | DontPushBackTime
                      deriving Eq

data TimedFrontier m = 
    TimedFrontier { tf_submit    :: !(Now -> Maybe Url -> [Url] -> m ())
                  , tf_nextUrl   :: !(Now -> m Result)
                  }

getMicrosToWait :: Now -> UTCTime -> Maybe Int
getMicrosToWait (Now now) allowed
    | now >= allowed = Nothing
    | otherwise =
        let later = diffUTCTime allowed now
        in Just . floor $ 1000000.0 * later
