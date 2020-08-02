{-# LANGUAGE OverloadedStrings #-}

module QueryParams where

import Types (CollectionName, parseCollectionName)

import Data.ByteString.Char8 (ByteString, unpack)
import Safe                  (headMay, readMay)

data QueryParams =
    QueryParams { query      :: !ByteString
                , maxResults :: !(Maybe Int)
                }

parseQueryParams :: [(ByteString, Maybe ByteString)] -> Maybe QueryParams
parseQueryParams raw = do

    mQuery <- headMay . map snd . filter (\x -> fst x == "q") $ raw 
    q <- mQuery

    pure $ QueryParams { query      = q
                       , maxResults = parseMaxResults
                       }

    where
    parseMaxResults :: Maybe Int
    parseMaxResults = do
        mMaxResults   <- headMay . map snd . filter (\x -> fst x == "n") $ raw 
        strMaxResults <- unpack <$> mMaxResults
        readMay strMaxResults

data MergeParams =
    MergeParams { dest :: !CollectionName
                , src  :: !CollectionName
                }

parseMergeParams :: [(ByteString, Maybe ByteString)] -> Maybe MergeParams
parseMergeParams raw = do
    src_  <- getCollectionName "src"
    dest_ <- getCollectionName "dst"
    pure $ MergeParams {dest = dest_, src = src_}

    where
    getCollectionName :: ByteString -> Maybe CollectionName
    getCollectionName key = do
        maybeVal <- headMay . map snd . filter (\x -> fst x == key) $ raw
        val      <- unpack <$> maybeVal
        parseCollectionName val
