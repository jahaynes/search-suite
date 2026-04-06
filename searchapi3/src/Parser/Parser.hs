{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings #-}

module Parser.Parser ( Parser(..)
                     , ParseResult(..)
                     ) where

import           Control.Applicative
import           Data.Functor        ((<&>))
import           Data.Text           (Text)

-- | Base parser type parameterized over a monad m
newtype Parser m s a =
    Parser { runParser :: s -> m (s, a) }

-- | Newtype wrapper for Either Text to provide Alternative instance
newtype ParseResult a = ParseResult { runParseResult :: Either Text a }
    deriving (Functor, Applicative, Monad)

instance Alternative ParseResult where
    empty = ParseResult (Left "empty")
    ParseResult (Left _) <|> b = b
    a <|> _ = a

-- Functor instance for Parser
instance (Functor m) => Functor (Parser m s) where
    fmap f (Parser run) =
        Parser $ \s -> (\(s', a) -> (s', f a)) <$> run s

-- Applicative instance for Parser
-- Note: Requires Monad m because state threading in <*> needs >>=
instance Monad m => Applicative (Parser m s) where
    pure x = Parser (\s -> pure (s, x))

    Parser pf <*> Parser px = Parser $ \s ->
        pf s >>= \(s', f) ->
        px s' <&> \(s'', x) -> (s'', f x)

-- Alternative instance for Parser
instance (Monad m, Alternative m) => Alternative (Parser m s) where
    empty = Parser (\_ -> empty)

    Parser pa <|> Parser pb = Parser $ \s ->
        pa s <|> pb s

-- Monad instance for Parser
instance (Monad m) => Monad (Parser m s) where
    return = pure

    Parser runA >>= pf = Parser $ \s ->
        runA s >>= \(s', a) ->
        let Parser runB = pf a
        in
        runB s'
