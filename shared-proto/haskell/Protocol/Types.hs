module Protocol.Types ( Input (..)
                      , InputDoc (..)
                      , IndexReply (..)
                      ) where

import Protocol.Encode

import Codec.CBOR.Decoding (decodeListLen, decodeWord32, decodeWord64)
import Codec.CBOR.Encoding (encodeListLen, encodeString, encodeWord32, encodeWord64)
import Data.Word           (Word32, Word64)
import Data.Text           (Text)
import Data.Vector         (Vector)

data InputDoc =
    InputDoc { url         :: !Text
             , content     :: !Text
             , compression :: !Text
             } deriving Show

instance Encode InputDoc where
    encode doc = mconcat [ encodeListLen 3
                         , encodeString $ url doc
                         , encodeString $ content doc
                         , encodeString $ compression doc
                         ]

data IndexReply =
    IndexReply { num_docs  :: !Word32
               , num_terms :: !Word32
               , ms_taken  :: !Word64
               } deriving Show

instance Encode IndexReply where
    encode ir = mconcat [ encodeListLen 3
                        , encodeWord32 $ num_docs ir
                        , encodeWord32 $ num_terms ir
                        , encodeWord64 $ ms_taken ir
                        ]

instance Decode IndexReply where
    decode = do
        3 <- decodeListLen
        IndexReply <$> decodeWord32
                   <*> decodeWord32
                   <*> decodeWord64

newtype Input =
    Input { docs :: Vector InputDoc }
        deriving Show

instance Encode Input where
    encode (Input ds) = encode ds
