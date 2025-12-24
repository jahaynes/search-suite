module Protocol.Encode where

import           Codec.CBOR.Decoding       (Decoder)
import           Codec.CBOR.Encoding       (Encoding, encodeListLen)
import           Codec.CBOR.Read           (deserialiseFromBytes)
import           Codec.CBOR.Write          (toLazyByteString, toStrictByteString)

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString, null)
import           Data.Vector               (Vector)
import qualified Data.Vector as V

cbor :: Encode a => a -> ByteString
cbor = toStrictByteString . encode

lcbor :: Encode a => a -> L.ByteString
lcbor = toLazyByteString . encode 

class Encode a where
    encode :: a -> Encoding

instance Encode a => Encode (Vector a) where
    encode xs = encodeListLen (fromIntegral $ V.length xs) <> mconcat (map encode $ V.toList xs)

class Decode a where
    decode :: Decoder s a

unlcbor :: Decode a => L.ByteString -> a
unlcbor bs =
    case deserialiseFromBytes decode bs of
        Left l                        -> error $ show l
        Right (rest, a) | L.null rest -> a
                        | otherwise   -> error "Leftover in unlcbor"
