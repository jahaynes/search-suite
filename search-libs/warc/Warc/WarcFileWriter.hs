{-# LANGUAGE LambdaCase
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
import           File

import           Control.Monad.IO.Class
import           Control.Monad               (forM_)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either                 (rights)
import           Data.Serialize              (encode)
import           Data.Word                   (Word64)
import           System.IO                   (hTell)

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
-- TODO check need sorted?
writeWarcFileImpl :: FilePath -> FilePath -> [WarcEntry] -> IO ()
writeWarcFileImpl destWarcFile destOffsets wes = runResourceT $ do
        offs <- openForWriting destOffsets
        warc <- openForWriting destWarcFile
        liftIO $ forM_ wes $ \we -> do
            -- Record in file.offs the warc entry's position 
            pos :: Word64 <- fromIntegral <$> hTell warc
            BS.hPut offs (encode pos)
            -- Write entry
            LBS.hPut warc (toLazyByteString we)

-- TODO what is behaviour of exception during (partial) acquire?
interleaveSortedWarcFilesAtPathImpl ::(String -> IO ()) -> FilePath -> FilePath -> FilePath -> IO ()
interleaveSortedWarcFilesAtPathImpl tempLogger x y dest = runResourceT $ do
    warcX <- openForReading (x <> "/file.warc")
    warcY <- openForReading (y <> "/file.warc")
    dOffs <- openForWriting (dest <> "/file.warc")
    dWarc <- openForWriting (dest <> "/file.offs")
    liftIO $ do
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

