{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Protocol.Example where

import Protocol.Types

eg0, eg1, eg2 :: Input
eg0 = Input { docs = [] }
eg1 = Input { docs = [inputDocExample] }
eg2 = Input { docs = [inputDocExample, inputDocExample] }

inputDocExample :: InputDoc
inputDocExample =
    InputDoc { compression = "none"
             , content     = "example content"
             , url         = "http://example/"
             }