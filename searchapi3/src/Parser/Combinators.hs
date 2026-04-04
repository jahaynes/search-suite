module Parser.Combinators where

import Parser.Parser (Parser(Parser))

import Control.Applicative (Alternative(..), many)
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Text           (Text)

reject :: Text -> Parser s a
reject msg = Parser $ \_ -> Left msg

sepBy1 :: Alternative f => f b -> f a -> f (NonEmpty a)
sepBy1 sep p = (:|) <$> p <*> many (sep *> p)
