{-# LANGUAGE OverloadedStrings #-}

module Query.QueryParser ( Clause (..)
                         , Op (..)
                         , parseQuery ) where

import Parser.Combinators
import Parser.LineLexer
import Parser.Parser

import           Control.Applicative         ((<|>))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Char                   (isSpace)
import           Data.Functor                (void)
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Text                   (Text)

data Clause = Conjunction !Op !(NonEmpty Clause)
            | ClauseText !ByteString
            | ClauseRegex !ByteString
                deriving Show

data Op = And | Or | Sub deriving (Eq, Show)

{- Example
/\ search
/\ \/ .cs
   \/ .ts
   \/ .js
/\ feature
-}

parseQuery :: ByteString -> Either Text Clause
parseQuery bs =
    case runParser parse (fromInput bs) of
        Right (ls, p)
            | C8.null (_input ls) -> Right p
            | otherwise           -> Left "Parse failure (leftover)"
        Left l                    -> Left l

-- Still to do.  Check AND-OR mismatches,  AND/ORs introduced out of nowhere
-- TODO - use a proper Writer for errors?
parse :: LineLexer Clause
parse = clauseOrText <* ws

    where
    clauseOrText :: LineLexer Clause
    clauseOrText = clause <|> regex <|> text

        where
        clause :: LineLexer Clause
        clause = do
            (col, op) <- junc
            Conjunction op <$> sepBy1 (matchJunc col op) clauseOrText

            where
            matchJunc :: Int -> Op -> LineLexer ()
            matchJunc col op = do
                (col', op') <- junc
                case (col == col', op == op') of
                    (False, _)    -> reject "not aligned"
                    (True, False) -> error "mismatch!" -- TODO: would like this reported
                    (True, True)  -> pure ()

        junc :: LineLexer (Int, Op)
        junc = ws *> ((,) <$> getCol <*> parseOp)
            where
            parseOp = (Or  <$ lexString "\\/")
                  <|> (And <$ lexString "/\\")
                  <|> (Sub <$ lexString "--")

    regex :: LineLexer Clause
    regex = do
        ws *> lexString "~"
        ws *> fmap ClauseRegex restOfLine

    text :: LineLexer Clause
    text = ws *> fmap ClauseText restOfLine

    ws :: LineLexer ()
    ws = void (lTakeWhile isSpace)
