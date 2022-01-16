{-# LANGUAGE DeriveGeneric,
             LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Snippets ( Snippets (..)
                , Snippet (..)
                , createSnippets ) where


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
import qualified Data.ByteString.Lazy as LBS
import           Data.Text                    (Text)
import           Data.Text.Encoding
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           System.IO
import           Text.HTML.TagSoup hiding (parseTags)
import           Text.HTML.TagSoup.Fast

data Snippets =
    Snippets { generateSnippets :: !(FilePath -> IO ())
             , mergeSnippets    :: !(FilePath -> FilePath -> FilePath -> IO ())
             , lookupSnippet    :: !(FilePath -> Text -> IO (Maybe Snippet))
             }

data Snippet = 
    Snippet { sn_uri     :: !Text
            , sn_snippet :: !Text
            } deriving (Generic, Show)

instance Serialise Snippet

createSnippets :: WarcFileReader -> Snippets
createSnippets wfr =
    Snippets { generateSnippets = generateSnippetsImpl wfr
             , mergeSnippets    = mergeSnippetsImpl
             , lookupSnippet    = lookupSnippetImpl
             }

lookupSnippetImpl :: FilePath -> Text -> IO (Maybe Snippet)
lookupSnippetImpl fp url = do

    let snippetsOffs = fp <> "/snippets.off"
    let snippetsFile = fp <> "/snippets.txt"

    hOffs <- openBinaryFile snippetsOffs ReadMode
    hSnip <- openBinaryFile snippetsFile ReadMode
    
    fs <- hFileSize hOffs
    let (numOffsets, 0) = (fs `divMod` 8)

    -- pairs (0,_), etc.
    snip :: Maybe (Text, Snippet) <- binarySearchM 0 (fromIntegral numOffsets - 1) url $ \i -> do

        -- Find the right offset
        hSeek hOffs AbsoluteSeek (fromIntegral $ i * 8)

        -- Read the offset + length
        eloff <- LBS.hGet hOffs 8
        let loff = decode eloff :: Word64
        eroff <- LBS.hGet hOffs 8
        let roff = decode eroff :: Word64
        let len = roff - loff

        -- Seek to the offset
        hSeek hSnip AbsoluteSeek (fromIntegral loff)
        snip :: Snippet <- deserialise <$> LBS.hGet hSnip (fromIntegral len)

        pure (sn_uri snip, snip)

    hClose hSnip
    hClose hOffs

    pure (snd <$> snip)

generateSnippetsImpl :: WarcFileReader -> FilePath -> IO ()
generateSnippetsImpl wfr x = do

    -- Input
    let warcFile = x <> "/file.warc"

    -- Output
    let snippetsOffs = x <> "/snippets.off"
    let snippetsFile = x <> "/snippets.txt"

    runResourceT $ do

        (releaseOffsets, handleOffsets) <- allocate
            (openBinaryFile snippetsOffs WriteMode)
            hClose

        (releaseDest, handleDest) <- allocate
            (openBinaryFile snippetsFile WriteMode)
            hClose

        _ <- liftIO $ do

            batchedRead wfr warcFile $ \wes ->
                forM_ wes $ \we@(WarcEntry headers _) ->
                    case summarise we of
                        -- Nothing -> pure ()
                        snippet -> do

                            -- Record in file.offs the snippets's position 
                            pos :: Word64 <- fromIntegral <$> hTell handleDest
                            LBS.hPut handleOffsets (encode pos)

                            -- Write snippet entry
                            let Just (StringValue uri) = getValue (MandatoryKey WarcRecordId) headers
                            LBS.hPut handleDest $ serialise Snippet { sn_uri = decodeUtf8 uri, sn_snippet = snippet }

            -- Write final offset
            pos :: Word64 <- fromIntegral <$> hTell handleDest
            LBS.hPut handleOffsets (encode pos)

        release releaseDest
        release releaseOffsets

-- TODO doesn't work if either is empty
mergeSnippetsImpl :: FilePath -> FilePath -> FilePath -> IO ()
mergeSnippetsImpl x y destPath = do

    let fpOff1   = x        <> "/snippets.off"
        fpDat1   = x        <> "/snippets.txt"
        fpOff2   = y        <> "/snippets.off"
        fpDat2   = y        <> "/snippets.txt"
        destOffs = destPath <> "/snippets.off"
        dest     = destPath <> "/snippets.txt"

    [off1, dat1, off2, dat2] <- mapM (`openFile` ReadMode) [fpOff1, fpDat1, fpOff2, fpDat2]

    hOff <- openFile destOffs WriteMode
    hDat <- openFile dest WriteMode

    reader1 <- reader off1 dat1
    reader2 <- reader off2 dat2 

    mergeReaders sn_uri reader1 reader2 $ \(x :: Snippet) -> do

        -- Write the offset
        off :: Word64 <- fromIntegral <$> hTell hDat
        LBS.hPut hOff (encode off)

        -- Write the data
        LBS.hPut hDat (serialise x)

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

summarise :: WarcEntry -> Text
summarise we@(WarcEntry _ (CompressedBody body)) = summarise $ decompress we
summarise    (WarcEntry _ (UncompressedBody body)) =

    let desc = map snd
             . concatMap (\(TagOpen "meta" attrs) -> filter (\(k,_) -> k == "content" ) attrs)
             . filter (\(TagOpen "meta" attrs) -> ("name", "description") `elem` attrs)
             . filter (isTagOpenName "meta")
             . takeWhile (not . isTagOpenName "body")
             . parseTags
             $ body

    in case desc of
        []    -> "No meta description available"
        (x:_) -> case decodeUtf8' x of
                    Left l  -> "Could not decode snippet"
                    Right r -> r
