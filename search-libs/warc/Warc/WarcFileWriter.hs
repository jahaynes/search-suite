{-# LANGUAGE LambdaCase
           , OverloadedStrings
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
                   , interleaveSortedWarcFilesAtPath = interleaveSortedWarcFilesAtPathImpl putStrLn -- TODO: better logger
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

-- TODO what is behaviour of exception during (partial) acquire?
interleaveSortedWarcFilesAtPathImpl ::(String -> IO ()) -> FilePath -> FilePath -> FilePath -> IO ()
interleaveSortedWarcFilesAtPathImpl tempLogger x y dest = do

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

        forM_ (merge entriesX entriesY) $ \case

            Left e -> tempLogger e

            Right we -> do

                -- Record in file.offs the warc entry's position 
                pos :: Word64 <- fromIntegral <$> hTell dWarc
                BS.hPut dOffs (encode pos)

                -- Write entry
                LBS.hPut dWarc (toLazyByteString we)


    where
    merge :: [WarcEntry] -> [WarcEntry] -> [Either String WarcEntry]
    merge xs [] = map Right xs
    merge [] ys = map Right ys
    merge (ex@(WarcEntry headerX _):xs)
          (ey@(WarcEntry headerY _):ys) =
        case getValue (MandatoryKey WarcRecordId) headerX of
            Just (StringValue recordX) ->
                case getValue (MandatoryKey WarcRecordId) headerY of
                    Just (StringValue recordY) -> 
                        case compare recordX recordY of
                            LT -> Right ex : merge     xs (ey:ys)
                            EQ -> Right ex : merge     xs     ys
                            GT -> Right ey : merge (ex:xs)    ys
                    _ -> Left "Bad WarcRecordId on RHS" : merge (ex:xs) ys
            _ -> Left "Bad WarcRecordId on LHS" : merge xs (ey:ys)

