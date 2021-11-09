{-# LANGUAGE OverloadedStrings #-}

module Page.Scrape where

import Page.Page             (Page (..))
import Url                   (Url, derelativise, mkUrl)

import Data.ByteString.Char8 (ByteString, unpack)
import Data.Maybe            (mapMaybe)
import Safe                  (headMay)
import Text.HTML.TagSoup

scrape :: Page -> [Url]
scrape (Page baseUrl body _) = do

    let tags = canonicalizeTags $ parseTags body

    let mBaseHref = mkUrl . unpack =<< findBaseHref tags

    mapMaybe (derelativise mBaseHref baseUrl . unpack . snd)
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
