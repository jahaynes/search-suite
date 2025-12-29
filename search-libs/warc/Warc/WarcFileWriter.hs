{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module WarcFileWriter ( WarcFileWriter (..)
                      , createWarcFileWriter
                      ) where

-- TODO, stop using lazy byte strings? (LBS)

import           Data.Warc.WarcEntry
import           Data.Warc.Header
import           Data.Warc.Key
import           Data.Warc.Value
import           Data.Warc.Parse

import           Control.Monad               (forM_)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either                 (rights)
import           Data.Serialize              (encode)
import           Data.Word                   (Word64)
import           System.IO
import           UnliftIO.Exception          (bracket)

data WarcFileWriter =
    WarcFileWriter { writeWarcFile                   :: !(FilePath -> FilePath -> [WarcEntry] -> IO ())
                   , interleaveSortedWarcFilesAtPath :: !(FilePath -> FilePath -> FilePath -> IO ())
                   }

createWarcFileWriter :: WarcFileWriter
createWarcFileWriter =
    WarcFileWriter { writeWarcFile                   = writeWarcFileImpl
                   , interleaveSortedWarcFilesAtPath = interleaveSortedWarcFilesAtPathImpl
                   }

-- TODO compress?
-- TODO exceptions
-- TODO check need sorted?
writeWarcFileImpl :: FilePath -> FilePath -> [WarcEntry] -> IO ()
writeWarcFileImpl destWarcFile destOffsets wes = do

    let acquire = do offs <- openBinaryFile destOffsets WriteMode
                     warc <- openBinaryFile destWarcFile WriteMode
                     pure (offs, warc)

    let release (offs, warc) = do hClose offs
                                  hClose warc

    bracket acquire release $ \(offs, warc) ->

        forM_ wes $ \we -> do

            -- Record in file.offs the warc entry's position 
            pos :: Word64 <- fromIntegral <$> hTell warc
            BS.hPut offs (encode pos)

            -- Write entry
            LBS.hPut warc (toLazyByteString we)

{- TODO lift this out
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
-}

-- TODO what is behaviour of exception during (partial) acquire?
interleaveSortedWarcFilesAtPathImpl :: FilePath -> FilePath -> FilePath -> IO ()
interleaveSortedWarcFilesAtPathImpl x y dest = do

    let sourceWarcX  = x    <> "/file.warc"
        sourceWarcY  = y    <> "/file.warc"
        destWarcFile = dest <> "/file.warc"
        destOffsets  = dest <> "/file.offs"

    let acquire = do warcX <- openBinaryFile sourceWarcX ReadMode
                     warcY <- openBinaryFile sourceWarcY ReadMode
                     dOffs <- openBinaryFile destOffsets WriteMode
                     dWarc <- openBinaryFile destWarcFile WriteMode
                     pure (warcX, warcY, dOffs, dWarc)

    let release (warcX, warcY, dOffs, dWarc) = do hClose warcX
                                                  hClose warcY
                                                  hClose dOffs
                                                  hClose dWarc

    bracket acquire release $ \(warcX, warcY, dOffs, dWarc) -> do

        entriesX <- rights . fromByteString <$> LBS.hGetContents warcX
        entriesY <- rights . fromByteString <$> LBS.hGetContents warcY

        forM_ (merge entriesX entriesY) $ \we -> do

            -- Record in file.offs the warc entry's position 
            pos :: Word64 <- fromIntegral <$> hTell dWarc
            BS.hPut dOffs (encode pos)

            -- Write entry
            LBS.hPut dWarc (toLazyByteString we)

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
