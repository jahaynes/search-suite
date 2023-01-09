{-# LANGUAGE OverloadedStrings #-}

import           Pipeline.FrontierTypes
import qualified Pipeline.TimedFrontier as TimedFrontier
import qualified Storage.MultiMap      as MM
import           Url

import           Gen.Time

import Control.Concurrent.STM
import Control.Monad           (when)
import Control.Monad.IO.Class  (liftIO)
import Data.Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range
import Hedgehog.Main

main :: IO ()
main = defaultMain [
         checkParallel $ Group "New.Frontier"
            [ ("testGetMicrosToWait", testGetMicrosToWait)
            , ("testTriggerRace"    , testTriggerRace)
            , ("testNextUrl"        , testNextUrl)
            , ("testMultiMap"       , testMultiMap)
            , ("testRelativeUrls"   , testRelativeUrls)
            , ("testBaseHrefs"      , testBaseHrefs)
            ]
       ]

testRelativeUrls :: Property
testRelativeUrls = property $
    let Just base = mkUrl "http://lambda-the-ultimate.org"
        rest      =       "node/user"
        Just url  = derelativise Nothing base rest
    in show url === "http://lambda-the-ultimate.org/node/user"

testBaseHrefs :: Property
testBaseHrefs = property $
    let mBaseHref = mkUrl "http://lambda-the-ultimate.org/"
        Just base = mkUrl "http://lambda-the-ultimate.org/node/user"
        rest      =       "archive/2020/05/04"
        Just url  = derelativise mBaseHref base rest
    in show url === "http://lambda-the-ultimate.org/archive/2020/05/04"

testMultiMap :: Property
testMultiMap = property $ do

    -- TODO turn into a bytestring pool and use elements to test collisions
    let genBytes = forAll $ Gen.bytes (constantFrom 0 0 100)

    key1 <- genBytes
    key2 <- genBytes
    val1 <- genBytes
    val2 <- genBytes

    mm1 <- liftIO MM.create

    (=== Nothing) =<< atomicallyIO (MM.mm_lookupMin mm1)

    (=== Just (key1, val1)) =<< atomicallyIO (do MM.mm_put mm1 key1 val1
                                                 MM.mm_lookupMin mm1)

    atomicallyIO (MM.mm_put mm1 key2 val2)
    when (key1 /= key2) $ do
        (=== Just (if key1 < key2 then (key1, val1) else (key2, val2))) =<< atomicallyIO (MM.mm_lookupMin mm1)
        (=== Just val1) =<< atomicallyIO (MM.mm_get mm1 key1)
        (=== Just val2) =<< atomicallyIO (MM.mm_get mm1 key2)

    mm2 <- liftIO MM.create
    atomicallyIO $ do MM.mm_put mm2 key1 val1
                      MM.mm_put mm2 key1 val2
                      MM.mm_put mm2 key2 val1
                      MM.mm_put mm2 key2 val2
    (=== Just (min val1 val2)) =<< atomicallyIO (MM.mm_get mm2 key1)
    (=== Just (min val1 val2)) =<< atomicallyIO (MM.mm_get mm2 key2)
    (=== Just (if key1 < key2 then (key1, min val1 val2) else (key2, min val1 val2))) =<< atomicallyIO (MM.mm_lookupMin mm2)

    (=== Just val2) =<< (atomicallyIO $ do MM.mm_delete mm2 key1 val1
                                           MM.mm_get mm2 key1)

    (=== Just val1) =<< atomicallyIO (do MM.mm_delete mm2 key2 val2
                                         MM.mm_get mm2 key2)

    (=== Nothing) =<< atomicallyIO (do MM.mm_delete mm2 key1 val2
                                       MM.mm_get mm2 key1)

    (=== Nothing) =<< atomicallyIO (do MM.mm_delete mm2 key2 val1
                                       MM.mm_get mm2 key2)

testGetMicrosToWait :: Property
testGetMicrosToWait = property $ do

    now <- forAll genTime
    let sooner = addUTCTime (-1) now
    let later  = addUTCTime   1  now

    getMicrosToWait (Now now) sooner === Nothing
    getMicrosToWait (Now now) later  === Just 1000000
    getMicrosToWait (Now now) now    === Nothing

testTriggerRace :: Property
testTriggerRace = property $ do

    Now now <- getNow
    let later = addUTCTime 1 now

    let Just url1 = mkUrl "https://www.url-one.com"
        Just url2 = mkUrl "https://www.url-two.com"

    frontier <- liftIO $ TimedFrontier.create 1

    tf_submit frontier (Now later) Nothing [url1]
    tf_submit frontier (Now later) Nothing [url2]

    (=== WaitMicros 1000000) =<< tf_nextUrl frontier (Now now)

testNextUrl :: Property
testNextUrl = property $ do

    Now time0_0 <- getNow
    let time0_1  = addUTCTime 0.1 time0_0
 
        time1_0  = addUTCTime 1   time0_0
        time1_1  = addUTCTime 1.1 time0_0

        time2_0  = addUTCTime 2   time0_0

    let Just url1 = mkUrl "https://www.same-host.com/1"
        Just url2 = mkUrl "https://www.diff-host.com/2"
        Just url3 = mkUrl "https://www.diff-host.com/3"

    frontier <- liftIO $ TimedFrontier.create 1

    -- Empty, therefore Done
    (=== Done) =<< tf_nextUrl frontier (Now time0_0)

    -- Submit url1
    tf_submit frontier (Now time0_1) Nothing [url1]

    -- Should return url1
    (=== Url url1) =<< tf_nextUrl frontier (Now time0_1)

    -- Even though it's too soon, should still get Done because there are no more URLs
    (=== Done) =<< tf_nextUrl frontier (Now time0_1)
    -- This is maybe not right.  Does it return Done if there are URLs on other hosts?  

    -- Submit url2, url3
    tf_submit frontier (Now time1_0) Nothing [url2]
    tf_submit frontier (Now time1_0) Nothing [url3]

    -- Should return url2
    (=== Url url2) =<< tf_nextUrl frontier (Now time1_0)

    --Too soon to return url2.  Wait 0.9 seconds.
    (=== WaitMicros 900000) =<< tf_nextUrl frontier (Now time1_1)

    -- Should return url3
    (=== Url url3) =<< tf_nextUrl frontier (Now time2_0)

getNow :: PropertyT IO Now
getNow = Now <$> liftIO getCurrentTime

atomicallyIO :: STM a -> PropertyT IO a
atomicallyIO = liftIO . atomically
