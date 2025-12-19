{-# LANGUAGE DeriveGeneric, DataKinds, OverloadedStrings #-}

module SharedProto ( ser
                   , example
                   , InputDoc(..)
                   , Input(..)
                   , inputDocExample
                   , inputExample
                   , IndexResult(..)
                   ) where

import Data.ByteString
import Data.ProtocolBuffers
import Data.Int                      (Int32)
import Data.Word                     (Word32, Word64)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
import Data.Serialize

data NewEvent =
    NewEvent { someBytes      :: !(Required 1 (Value ByteString))
             , someText       :: !(Required 2 (Value Text))
             , someNum        :: !(Required 3 (Value Int32))
             , metadataKeys   :: !(Repeated 4 (Value Text))
             , metadataValues :: !(Repeated 5 (Value Text))
             } deriving (Generic, Show)

instance Encode NewEvent
instance Decode NewEvent

example :: NewEvent
example =
    NewEvent { someBytes      = putField ("foo" :: ByteString)
             , someText       = putField ("bar" :: Text)
             , someNum        = putField 234
             , metadataKeys   = putField ["key1", "key2"]
             , metadataValues = putField ["value1", "value2"]
             }

ser :: Encode a => a -> ByteString
ser = runPut . encodeMessage

-- InputDoc and Input mirror the protobuf messages used by the indexer
data InputDoc =
    InputDoc { url         :: !(Required 1 (Value Text))
             , content     :: !(Required 2 (Value Text))
             , compression :: !(Optional 3 (Value Text))
             } deriving (Generic, Show)

instance Encode InputDoc
instance Decode InputDoc

data Input =
    Input { docs :: !(Repeated 1 (Message InputDoc)) }
    deriving (Generic, Show)

instance Encode Input
instance Decode Input

inputDocExample :: InputDoc
inputDocExample =
    InputDoc { url = putField ("http://example/" :: Text)
             , content = putField ("example content" :: Text)
             , compression = putField Nothing
             }

inputExample :: Input
inputExample =
    Input { docs = putField [inputDocExample] }


-- IndexResult mirrors the indexer reply (num_docs, num_terms, ms_taken)
data IndexResult =
    IndexResult { numDocs  :: !(Required 1 (Value Word32))
                , numTerms :: !(Required 2 (Value Word32))
                , msTaken  :: !(Required 3 (Value Word64))
                } deriving (Generic, Show)

instance Encode IndexResult
instance Decode IndexResult
