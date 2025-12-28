module Protocol.Encode ( Decode (..)
                       , Encode (..)
                       , lcbor
                       , unlcbor
                       ) where

import           Codec.CBOR.Decoding       (Decoder)
import           Codec.CBOR.Encoding       (Encoding, encodeListLen)
import           Codec.CBOR.Read           (deserialiseFromBytes)
import           Codec.CBOR.Write          (toLazyByteString)

import qualified Data.ByteString.Lazy as L (ByteString, null)
import           Data.Vector               (Vector)
import qualified Data.Vector as V

lcbor :: Encode a => a -> L.ByteString
lcbor = toLazyByteString . cborEncode

class Encode a where
    cborEncode :: a -> Encoding

instance Encode a => Encode (Vector a) where
    cborEncode xs = encodeListLen (fromIntegral $ V.length xs) <> mconcat (map cborEncode $ V.toList xs)

class Decode a where
    cborDecode :: Decoder s a

unlcbor :: Decode a => L.ByteString -> a
unlcbor bs =
    case deserialiseFromBytes cborDecode bs of
        Left l                        -> error $ show l
        Right (rest, a) | L.null rest -> a
                        | otherwise   -> error "Leftover in unlcbor"
