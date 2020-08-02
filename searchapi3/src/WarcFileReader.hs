{-# LANGUAGE OverloadedStrings #-}

module WarcFileReader  where

import           Data.Warc.Parse                  (fromByteStringRemainder {- :: L.ByteString -> Either ByteString (Int, WarcEntry, L.ByteString) -} )
import           Data.Warc.WarcEntry              (WarcEntry (..))
import           Data.Warc.Value
import           Data.Warc.Header
import           Data.Warc.Key


import           Control.Exception.Safe           (catchIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Int                         (Int64)
import           Data.Serialize
import           Data.Word                        (Word64)
import           Data.Vector                      (Vector)
import qualified Data.Vector                as V
import           System.Directory                 (getFileSize)
import           System.IO
import           Text.Printf                      (printf)

data WarcFileReader =

    WarcFileReader { batchedRead :: !(FilePath
                                 ->  (Vector WarcEntry -> IO ())
                                 ->  IO (Either ByteString ()))

                   , findWarcEntry :: !(FilePath -> FilePath -> ByteString -> IO (Maybe WarcEntry))

                   }

createWarcFileReader :: Int
                     -> (ByteString -> IO ())
                     -> WarcFileReader
createWarcFileReader batchSize logger =
    WarcFileReader { batchedRead   = batchedReadImpl batchSize logger
                   , findWarcEntry = findWarcEntryImpl
                   }

-- TODO use a tighter structure than (offs,fullwarc).  Perhaps (offs,urls-warcoffs,fullwarc))
-- TODO resourceT
findWarcEntryImpl :: FilePath -> FilePath -> ByteString -> IO (Maybe WarcEntry)
findWarcEntryImpl warcFile warcOffs url = do

    hWarc <- openBinaryFile warcFile ReadMode
    hOff <- openBinaryFile warcOffs ReadMode
    fs <- hFileSize hOff
    let (numOffsets, 0) = fs `divMod` 8

    x <- binarySearchM 0 (fromIntegral numOffsets) url $ \i -> do

        -- Find the right offset
        hSeek hOff AbsoluteSeek (fromIntegral $ i * 8)
        bsOff <- BS.hGetSome hOff 8
        let Right off = decode bsOff :: Either String Word64

        -- Seek to the offset
        hSeek hWarc AbsoluteSeek (fromIntegral off)
        warcData <- LBS.hGetContents hWarc

        -- Parse one entry and return its url
        let Right (_, we@(WarcEntry header _), _) = fromByteStringRemainder warcData
        let Just (StringValue recordId) = getValue (MandatoryKey WarcRecordId) header
        pure (recordId, we)

    hClose hWarc
    hClose hOff

    pure (snd <$> x)

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

batchedReadImpl :: Int
                -> (ByteString -> IO ())
                -> FilePath
                -> (Vector WarcEntry -> IO ())
                -> IO (Either ByteString ())
batchedReadImpl batchSize logger warcFile action = do

    sz <- fromIntegral <$> getFileSize warcFile
    h <- openFile warcFile ReadMode

    let job = do contents <- L8.hGetContents h
                 go sz 0 0 [] contents
                 hClose h
                 pure $ Right ()

        handler ioe = do let errMsg = C8.pack $ show ioe
                         logger errMsg
                         hClose h
                         pure $ Left errMsg

    catchIO job handler

    where
    go fileSz szRead n acc contents

        | L8.null contents =
            flush acc 0

        | n == batchSize = do
            flush acc (fileSz - szRead)
            go fileSz szRead 0 [] contents

        | otherwise =

            case fromByteStringRemainder contents of

                Left err ->
                    logger $ C8.pack err

                Right (szRead', we, remainder) ->
                    go fileSz (szRead + fromIntegral szRead') (n + 1) (we:acc) remainder

        where
        flush :: [WarcEntry] -> Int64 -> IO ()
        flush wes remainingBytes = do

            let vWes     = V.reverse . V.fromListN n $ wes
                docsRead = V.length vWes
                complete = 100.0 * fromIntegral (fileSz - remainingBytes) / fromIntegral fileSz :: Float

            logger . C8.pack $
                printf "Read %d warc entries.  MB left %d.  Complete %.2f%%" docsRead (remainingBytes `div` (1024 * 1024)) complete

            action vWes