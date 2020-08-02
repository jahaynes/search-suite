{-# LANGUAGE OverloadedStrings #-}

module Page.Scrape where

import Page.Page             (Page (..))
import Url                   (Url, derelativise)

import Data.ByteString.Char8 (unpack)
import Data.CaseInsensitive  (mk)
import Data.Maybe            (mapMaybe)
import Text.HTML.TagSoup

scrape :: Page -> [Url]
scrape (Page baseUrl body _) = mapMaybe (derelativise baseUrl . unpack . snd) 
                             . filter (\(k,_) -> mk k == mk "href")
                             . concatMap (\(TagOpen _ attribs) -> attribs)
                             . filter (isTagOpenName "a")
                             . parseTags
                             $ body
