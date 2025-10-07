{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import           Control.Applicative
import           Data.ByteString             (ByteString)
import           Data.Functor                ((<&>))

newtype Parser s a =
    Parser { runParser :: s -> Either ByteString (s, a) }

instance Functor (Parser s) where

    fmap f (Parser run) =
        Parser $ \s -> run s <&> \(s', a) -> (s', f a)

instance Applicative (Parser s) where

    pure x = Parser (\s -> Right (s, x))

    Parser pf <*> Parser px = Parser $ \s ->
        case pf s of
            Left l -> Left l
            Right (s', f) ->
                case px s' of
                    Left l -> Left l
                    Right (s'', x) -> Right (s'', f x)

instance Alternative (Parser s) where

    empty = Parser (\_ -> Left "No more Alternatives")

    Parser pa <|> Parser pb = Parser $ \s ->
        case pa s of
            Left{} -> pb s
            r      -> r

instance Monad (Parser s) where

    return = pure

    Parser runA >>= pf = Parser $ \s ->
        case runA s of
            Left l        -> Left l
            Right (s', a) ->
                let Parser runB = pf a
                in
                runB s'
