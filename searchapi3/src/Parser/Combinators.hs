{-# LANGUAGE FlexibleContexts #-}

module Parser.Combinators where

import Parser.Transformer

import Control.Applicative (Alternative(empty), many)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Text           (Text)

-- TODO looks wrong.  Can be a monadfail perhaps?
reject :: (Monad m, Alternative (ParserT s m)) => Text -> ParserT s m a
reject _ = empty

sepBy1 :: (Monad m, Alternative (ParserT s m)) => ParserT s m b -> ParserT s m a -> ParserT s m (NonEmpty a)
sepBy1 sep p = (:|) <$> p <*> many (sep *> p)