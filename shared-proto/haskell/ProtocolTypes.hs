module ProtocolTypes ( Input (..)
                     , InputDoc (..)
                     , IndexReply (..)
                     ) where

import Encode

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding (encodeString, encodeWord32, encodeWord64)
import Data.Word           (Word32, Word64)
import Data.Text           (Text)
import Data.Vector         (Vector)

data InputDoc =
    InputDoc { url         :: !Text
             , content     :: !Text
             , compression :: !Text
             } deriving Show

instance Encode InputDoc where
    encode doc = mconcat [ encodeString $ url doc
                         , encodeString $ content doc
                         , encodeString $ compression doc
                         ]

data IndexReply =
    IndexReply { num_docs  :: !Word32
               , num_terms :: !Word32
               , ms_taken  :: !Word64
               } deriving Show

instance Encode IndexReply where
    encode ir = mconcat [ encodeWord32 $ num_docs ir
                        , encodeWord32 $ num_terms ir
                        , encodeWord64 $ ms_taken ir
                        ]

instance Decode IndexReply where
    decode =
        IndexReply <$> decodeWord32
                   <*> decodeWord32
                   <*> decodeWord64

newtype Input =
    Input { docs :: Vector InputDoc }
        deriving Show

instance Encode Input where
    encode (Input ds) = encode ds
