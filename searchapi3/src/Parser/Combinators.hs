module Parser.Combinators where

import Parser.Parser

import Control.Applicative         (many)
import Data.ByteString             (ByteString)
import Data.List.NonEmpty          (NonEmpty (..))

reject :: ByteString -> Parser s a
reject msg = Parser $ \_ -> Left msg

sepBy1 :: Parser s b -> Parser s a -> Parser s (NonEmpty a)
sepBy1 sep p = (:|) <$> p <*> many (sep *> p)
