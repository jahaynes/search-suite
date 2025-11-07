module Parser.Combinators where

import Parser.Parser

import Control.Applicative (many)
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Text           (Text)

reject :: Text -> Parser s a
reject msg = Parser $ \_ -> Left msg

sepBy1 :: Parser s b -> Parser s a -> Parser s (NonEmpty a)
sepBy1 sep p = (:|) <$> p <*> many (sep *> p)
