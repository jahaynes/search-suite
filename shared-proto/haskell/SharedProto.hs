{-# LANGUAGE DeriveGeneric, DataKinds, OverloadedStrings #-}

module SharedProto  where

import Data.ByteString
import Data.ProtocolBuffers
import Data.Word                     (Word32, Word64)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
import Data.Serialize

gf :: HasField a => a -> FieldType a
gf = getField

ser :: Encode a => a -> ByteString
ser = runPut . encodeMessage

deser :: Decode a => ByteString -> Either String a
deser bs = runGet decodeMessage bs

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

eg0, eg1, eg2 :: Input
eg0 = Input { docs = putField [] }
eg1 = Input { docs = putField [inputDocExample] }
eg2 = Input { docs = putField [inputDocExample, inputDocExample] }


-- IndexResult mirrors the indexer reply (num_docs, num_terms, ms_taken)
data IndexResult =
    IndexResult { numDocs  :: !(Required 1 (Value Word32))
                , numTerms :: !(Required 2 (Value Word32))
                , msTaken  :: !(Optional 3 (Value Word64))
                } deriving (Generic, Show)

instance Encode IndexResult
instance Decode IndexResult
