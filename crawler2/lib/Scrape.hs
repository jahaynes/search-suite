{-# LANGUAGE OverloadedStrings #-}

module Scrape ( scrapeUrls ) where

import Restful.Types

import Data.ByteString.Char8 (ByteString, unpack)
import Data.Maybe            (mapMaybe)
import Text.HTML.TagSoup

scrapeUrls :: Response -> [Url]
scrapeUrls response =

    let baseUrl   = getUrl response
        body      = getBody response
        tags      = canonicalizeTags $ parseTags body
        mBaseHref = mkUrl . unpack =<< findBaseHref tags

    in mapMaybe (derelativise mBaseHref baseUrl . unpack . snd)
                . filter (\(k,_) -> k == "href")
                . concatMap (\(TagOpen _ attribs) -> attribs)
                . filter (isTagOpenName "a")
                . parseTags
                $ body

    where
    findBaseHref :: [Tag ByteString] -> Maybe ByteString
    findBaseHref = headMay
                 . map snd
                 . filter (\(k,_) -> k == "href")
                 . concatMap (\(TagOpen _ attribs) -> attribs)
                 . filter (isTagOpenName "base")

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x