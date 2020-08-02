module Gen.Time where

import           Data.Time
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

genTime :: Gen UTCTime
genTime =
    UTCTime <$> genDay
            <*> genDayTime
    where
    genDay = ModifiedJulianDay <$> Gen.integral (Range.constantFrom 2000 1970 2100)
    genDayTime = secondsToDiffTime <$> Gen.integral (Range.constantFrom 0 0 86401)

