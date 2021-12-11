{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module WarcFileWriter ( WarcFileWriter (..)
                      , createWarcFileWriter ) where

import           Api
import           Component
import           Data.Warc.WarcEntry    
import           Data.Warc.Body
import           Data.Warc.Header
import           Data.Warc.Key
import           Data.Warc.Value
import           Data.Warc.Parse
import           Data.Serialize
import           Types

import           Control.Monad                       (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy         as LBS
import           Data.Either                         (rights)
import           Data.Text.Encoding
import           Data.Word                           (Word64)
import           System.IO

data WarcFileWriter =
    WarcFileWriter { writeWarcFile       :: !(FilePath -> FilePath -> [Doc] -> IO ())
                   , interleaveWarcFiles :: !(Component -> Component -> FilePath -> IO ())
                   }

createWarcFileWriter :: WarcFileWriter
createWarcFileWriter =
    WarcFileWriter { writeWarcFile       = writeWarcFileImpl
                   , interleaveWarcFiles = interleaveWarcFilesSortedImpl
                   }

-- TODO compress?
-- TODO exceptions
writeWarcFileImpl :: FilePath -> FilePath -> [Doc] -> IO ()
writeWarcFileImpl destWarcFile destOffsets ds = runResourceT $ do

        (releaseOffsets, handleOffsets) <- allocate
            (openBinaryFile destOffsets WriteMode)
            hClose

        (releaseDest, handleDest) <- allocate
            (openBinaryFile destWarcFile WriteMode)
            hClose

        liftIO $ forM_ (map fromDoc ds) $ \we -> do

            -- Record in file.offs the warc entry's position 
            pos :: Word64 <- fromIntegral <$> hTell handleDest
            BS.hPut handleOffsets (encode pos)

            -- Write entry
            LBS.hPut handleDest (toLazyByteString we)

        release releaseDest
        release releaseOffsets

fromDoc :: Doc -> WarcEntry
fromDoc (Doc url content) =
    let body = encodeUtf8 content
        len  = C8.length body
        url' = encodeUtf8 url
        header = setValue (MandatoryKey WarcRecordId)  (Just $ StringValue url')
               . setValue (MandatoryKey ContentLength) (Just $ IntValue len)
               . setValue (MandatoryKey WarcType)      (Just $ StringValue "response")
               $ WarcHeader (WarcVersion "1.0") []
    in compress $ WarcEntry header (UncompressedBody body)

interleaveWarcFilesSortedImpl :: Component -> Component -> FilePath -> IO ()
interleaveWarcFilesSortedImpl x y dest = do

    let sourceWarcX  = path x <> "/" <> "file.warc"
        sourceWarcY  = path y <> "/" <> "file.warc"
        destWarcFile = dest   <> "/" <> "file.warc"
        destOffsets  = dest   <> "/" <> "file.offs"

    runResourceT $ do

        (releaseX, handleX) <- allocate
            (openBinaryFile sourceWarcX ReadMode)
            hClose
    
        (releaseY, handleY) <- allocate
            (openBinaryFile sourceWarcY ReadMode)
            hClose

        (releaseOffsets, handleOffsets) <- allocate
            (openBinaryFile destOffsets WriteMode)
            hClose

        (releaseDest, handleDest) <- allocate
            (openBinaryFile destWarcFile WriteMode)
            hClose

        entriesX <- rights . fromByteString <$> liftIO (LBS.hGetContents handleX)
        entriesY <- rights . fromByteString <$> liftIO (LBS.hGetContents handleY)

        liftIO $ forM_ (merge entriesX entriesY) $ \we -> do

            -- Record in file.offs the warc entry's position 
            pos :: Word64 <- fromIntegral <$> hTell handleDest
            BS.hPut handleOffsets (encode pos)

            -- Write entry
            LBS.hPut handleDest (toLazyByteString we)

        release releaseDest
        release releaseOffsets
        release releaseX
        release releaseY

    where
    merge :: [WarcEntry] -> [WarcEntry] -> [WarcEntry]
    merge xs [] = xs
    merge [] ys = ys
    merge (ex@(WarcEntry headerX _):xs)
          (ey@(WarcEntry headerY _):ys) =

        let Just (StringValue recordX) = getValue (MandatoryKey WarcRecordId) headerX
            Just (StringValue recordY) = getValue (MandatoryKey WarcRecordId) headerY

        in case compare recordX recordY of
            LT -> ex : merge     xs (ey:ys)
            EQ -> ex : merge     xs     ys
            GT -> ey : merge (ex:xs)    ys
