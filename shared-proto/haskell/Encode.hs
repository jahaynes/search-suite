{-# LANGUAGE RankNTypes #-}

module Encode where

import           Codec.CBOR.Decoding       (Decoder)
import           Codec.CBOR.Encoding       (Encoding)
import           Codec.CBOR.Read           (deserialiseFromBytes)
import           Codec.CBOR.Write          (toLazyByteString, toStrictByteString)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import           Data.Vector               (Vector)
import qualified Data.Vector as V

cbor :: Encode a => a -> ByteString
cbor = toStrictByteString . encode

lcbor :: Encode a => a -> L.ByteString
lcbor = toLazyByteString . encode 

class Encode a where
    encode :: a -> Encoding

-- TODO - check if vectors are actually prefereable to Lists in call sites
instance Encode a => Encode (Vector a) where
    encode = mconcat . map encode . V.toList


class Decode a where
    decode :: Decoder s a

--class Decoder a where
  --  decoder :: Encoding -> a


uncbor = deserialiseFromBytes . decode

{-



uncbor :: (forall s. Decoder s a) -> L.ByteString -> Either String (ByteString, a) 
uncbor d bs =
    case deserialiseFromBytes d bs of
        Left{} -> undefined
        Right{} -> undefined

uncbor2 :: (forall s. Decoder s a) -> L.ByteString -> Either String (ByteString, a) 
uncbor2 d bs = do

    let d = decode 

    case deserialiseFromBytes d bs of
        Left{} -> undefined
        Right{} -> undefined

-}