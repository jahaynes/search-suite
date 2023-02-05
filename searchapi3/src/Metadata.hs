{-# LANGUAGE DeriveGeneric,
             LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Metadata ( Metadata (..)
                , MetadataApi (..)
                , createMetadataApi ) where

import           Data.Warc.Body
import           Data.Warc.Header
import           Data.Warc.Key
import           Data.Warc.Value
import           Data.Warc.WarcEntry ( WarcEntry (..), decompress)
import           Util.BinarySearch
import           WarcFileReader

import           Codec.Serialise              (Serialise, deserialise, serialise)
import           Control.Monad                (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Binary                  (decode, encode)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import           Data.Char                    (isSpace)
import           Data.Map                     (Map)
import qualified Data.Map as M
import           Data.Maybe                   (mapMaybe, maybeToList)
import           Data.Text                    (Text)
import           Data.Text.Encoding
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           System.IO
import           Text.HTML.TagSoup

data MetadataApi =
    MetadataApi { generateMetadata :: !(FilePath -> IO ())
                , mergeMetadata    :: !(FilePath -> FilePath -> FilePath -> IO ())
                , lookupMetadata   :: !(FilePath -> Text -> IO (Maybe Metadata))
                }

-- TODO: speedup opportunity
-- Keep the uri separate, for less serialisation in the binary search
newtype Metadata =
    Metadata { unMetadata :: Map Text Text}
        deriving Generic

instance Serialise Metadata

createMetadataApi :: WarcFileReader -> MetadataApi
createMetadataApi wfr =
    MetadataApi { generateMetadata = generateMetadataImpl wfr
                , mergeMetadata    = mergeMetadataImpl
                , lookupMetadata   = lookupMetadataImpl
                }

-- TODO resourceT
lookupMetadataImpl :: FilePath -> Text -> IO (Maybe Metadata)
lookupMetadataImpl fp url = do

    let metadataOffs = fp <> "/metadata.off"
    let metadataFile = fp <> "/metadata.txt"

    hOffs <- openBinaryFile metadataOffs ReadMode
    hMeta <- openBinaryFile metadataFile ReadMode
    
    fs <- hFileSize hOffs
    let (numOffsets, 0) = (fs `divMod` 8)

    -- pairs (0,_), etc.
    meta :: Maybe (Text, Metadata) <- binarySearchM 0 (fromIntegral numOffsets - 1) url $ \i -> do

        -- Find the right offset
        hSeek hOffs AbsoluteSeek (fromIntegral $ i * 8)

        -- Read the offset + length
        loff :: Word64 <- decode <$> LBS.hGet hOffs 8
        roff :: Word64 <- decode <$> LBS.hGet hOffs 8
        let len = roff - loff

        -- Seek to the offset
        hSeek hMeta AbsoluteSeek (fromIntegral loff)
        meta :: Metadata <- deserialise <$> LBS.hGet hMeta (fromIntegral len)

        pure (uriOf meta, meta)

    hClose hMeta
    hClose hOffs

    pure (snd <$> meta)

generateMetadataImpl :: WarcFileReader -> FilePath -> IO ()
generateMetadataImpl wfr x = do

    -- Input
    let warcFile = x <> "/file.warc"

    -- Output
    let metadataOffs = x <> "/metadata.off"
    let metadataFile = x <> "/metadata.txt"

    runResourceT $ do

        (releaseOffsets, handleOffsets) <- allocate
            (openBinaryFile metadataOffs WriteMode)
            hClose

        (releaseDest, handleDest) <- allocate
            (openBinaryFile metadataFile WriteMode)
            hClose

        _ <- liftIO $ do

            batchedRead wfr warcFile $ \wes ->
                forM_ wes $ \we@(WarcEntry headers _) -> do

                    -- Record in file.offs the metadata's position 
                    pos :: Word64 <- fromIntegral <$> hTell handleDest
                    LBS.hPut handleOffsets (encode pos)

                    -- Write metadata entry
                    let Just (StringValue uri) = getValue (MandatoryKey WarcRecordId) headers
                    let Metadata mm = scrapeMetadata we
                    let metadata = Metadata $ M.insert "uri" (decodeUtf8 uri) mm
                    LBS.hPut handleDest . serialise $ metadata

            -- Write final offset
            pos :: Word64 <- fromIntegral <$> hTell handleDest
            LBS.hPut handleOffsets (encode pos)

        release releaseDest
        release releaseOffsets

-- all metadata must have uri
uriOf :: Metadata -> Text
uriOf (Metadata mm) =
    case M.lookup "uri" mm of
        Nothing  -> error "Metadata without uri!"
        Just uri -> uri

-- TODO doesn't work if either is empty
mergeMetadataImpl :: FilePath -> FilePath -> FilePath -> IO ()
mergeMetadataImpl x y destPath = do

    let fpOff1   = x        <> "/metadata.off"
        fpDat1   = x        <> "/metadata.txt"
        fpOff2   = y        <> "/metadata.off"
        fpDat2   = y        <> "/metadata.txt"
        destOffs = destPath <> "/metadata.off"
        dest     = destPath <> "/metadata.txt"

    [off1, dat1, off2, dat2] <- mapM (`openFile` ReadMode) [fpOff1, fpDat1, fpOff2, fpDat2]

    hOff <- openFile destOffs WriteMode
    hDat <- openFile dest WriteMode

    reader1 <- reader off1 dat1
    reader2 <- reader off2 dat2 

    mergeReaders uriOf reader1 reader2 $ \(md :: Metadata) -> do

        -- Write the offset
        off :: Word64 <- fromIntegral <$> hTell hDat
        LBS.hPut hOff (encode off)

        -- Write the data
        LBS.hPut hDat (serialise md)

    -- Write the final offset
    off :: Word64 <- fromIntegral <$> hTell hDat
    LBS.hPut hOff (encode off)

    hClose hDat
    hClose hOff

    mapM_ hClose [off1, dat1, off2, dat2]

mergeReaders :: Ord c => (a -> c)
                      -> (Word64 -> IO (Maybe (Word64, a)))
                      -> (Word64 -> IO (Maybe (Word64, a)))
                      -> (a -> IO ())
                      -> IO ()
mergeReaders cmp reader1 reader2 fun =
    go1 0 0 Nothing

    where
    go1 off1 off2 Nothing =
        reader1 off1 >>= \case
            Nothing         -> drain reader2 off2
            Just (off1', x) -> go2 off1' off2 (Just x)

    go1 off1 off2 (Just prev2) = 
        reader1 off1 >>= \case
            Nothing          -> fun prev2 >> drain reader2 off2
            Just (off1', x1) ->
                case compare (cmp prev2) (cmp x1) of
                    LT -> fun prev2 >> go2 off1' off2 (Just x1)
                    EQ -> fun prev2 >> go2 off1' off2 Nothing
                    GT -> fun x1    >> go1 off1' off2 (Just prev2)

    go2 off1 off2 Nothing =
        reader2 off2 >>= \case
            Nothing          -> drain reader1 off1
            Just (off2', x2) -> go1 off1 off2' (Just x2)

    go2 off1 off2 (Just prev1) = 
        reader2 off2 >>= \case
            Nothing          -> fun prev1 >> drain reader1 off1
            Just (off2', x2) ->
                case compare (cmp prev1) (cmp x2) of
                    LT -> fun prev1 >> go1 off1 off2' (Just x2)
                    EQ -> fun prev1 >> go1 off1 off2' Nothing
                    GT -> fun x2    >> go2 off1 off2' (Just prev1)

    drain rdr off =
        rdr off >>= \case
            Nothing -> pure ()
            Just (off', t) -> fun t >> drain rdr off'

reader :: Serialise a => Handle
                      -> Handle
                      -> IO (Word64 -> IO (Maybe (Word64, a)))
reader hOff hDat = do
    _ <- LBS.hGet hOff 8 -- skip first offset (0)
    pure $ \lastOff -> do
        eof <- hIsEOF hOff
        if eof
            then pure Nothing
            else do
                off2 <- decode <$> LBS.hGet hOff 8
                x <- deserialise <$> LBS.hGet hDat (fromIntegral $ off2 - lastOff)
                pure $ Just (off2, x)

-- TODO - these summaries are only for HTML
-- should be contextual
-- e.g. show public classes for Java?
-- TODO - filter some interesting subset of possible html metadata names?
scrapeMetadata :: WarcEntry -> Metadata
scrapeMetadata we@(WarcEntry _ (CompressedBody _)) = scrapeMetadata $ decompress we
scrapeMetadata    (WarcEntry _ (UncompressedBody body)) =

    let headTags = getGroup "head"
                 . parseTags
                 $ body

        mTitle = do
            t <- textFrom $ getGroup "title" headTags
            pure ("title", t)

    in Metadata $  M.fromList
                (  maybeToList mTitle
                ++ mapMaybe metaNameContentPair headTags )

    -- TODO title is just a tag, not metadata
    -- TODO failing to get meta description, start reading from first body/div/p?
    -- Also read title from meta?  If not then use first H?, or filename?

    where
    getGroup :: Eq a => a -> [Tag a] -> [Tag a]
    getGroup name = takeWhile (not . isTagCloseName name)
                  . tail'
                  . dropWhile (not . isTagOpenName name)

    textFrom :: [Tag ByteString] -> Maybe Text
    textFrom ts = rightToMaybe . decodeUtf8'
              =<< notAllSpace (innerText $ filter isTagText ts)

    metaNameContentPair :: Tag ByteString -> Maybe (Text, Text)
    metaNameContentPair t@(TagOpen "meta" _) = do
        (n, c) <- case (fromAttrib "name" t, fromAttrib "content" t) of
                      ("",  _) -> Nothing
                      ( _, "") -> Nothing
                      nc       -> Just nc
        n' <- rightToMaybe $ decodeUtf8' n
        c' <- rightToMaybe $ decodeUtf8' c
        pure (n', c')
    metaNameContentPair _ = Nothing

rightToMaybe :: Either l r -> Maybe r
rightToMaybe Left{} = Nothing
rightToMaybe (Right r) = Just r

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail'     [] = []

notAllSpace :: ByteString -> Maybe ByteString
notAllSpace x | C8.all isSpace x = Nothing
              | otherwise        = Just x
