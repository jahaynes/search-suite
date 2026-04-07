{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings #-}

module Parser.Parser ( Parser(..) ) where

import           Control.Applicative
import           Data.Functor        ((<&>))

newtype Parser m s a =
    Parser { runParser :: s -> m (s, a) }

instance Functor m => Functor (Parser m s) where
    fmap f (Parser run) =
        Parser $ \s -> (\(s', a) -> (s', f a)) <$> run s

instance Monad m => Applicative (Parser m s) where
    pure x = Parser (\s -> pure (s, x))

    Parser pf <*> Parser px = Parser $ \s ->
        pf s >>= \(s', f) ->
        px s' <&> \(s'', x) -> (s'', f x)

instance (Monad m, Alternative m) => Alternative (Parser m s) where
    empty = Parser (\_ -> empty)

    -- Is this right?
    Parser pa <|> Parser pb = Parser $ \s ->
        pa s <|> pb s

instance Monad m => Monad (Parser m s) where
    return = pure

    Parser runA >>= pf = Parser $ \s ->
        runA s >>= \(s', a) ->
        let Parser runB = pf a
        in
        runB s'
