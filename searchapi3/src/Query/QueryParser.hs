{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Query.QueryParser ( Clause (..)
                         , Op (..)
                         , lexQuery
                         , lexAndParse
                         , parseQuery ) where

import Parser.Combinators
import Parser.LineLexer
import Parser.Parser

import           Control.Applicative         ((<|>), many)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Char                   (isSpace)
import           Data.Functor                (void)
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Text                   (Text)
import qualified Data.Text as T

data Clause = Conjunction !Op !(NonEmpty Clause)
            | ClauseText !ByteString
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

lexAndParse :: ByteString -> Either Text Clause
lexAndParse bs =
    lexQuery bs >>=
        parseTokens

parseTokens :: [Token] -> Either Text Clause
parseTokens ts =
    case runParser parse' ts of
        Right ([], p) -> Right p
        Right (ts, p) -> Left . T.pack $ ("Partial parse:\n" ++ show ts ++ "\n" ++ show p)
        Left l        -> Left l

lexQuery :: ByteString -> Either Text [Token]
lexQuery bs =
    case runParser lexer (fromInput bs) of
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
    clauseOrText = clause <|> text

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

    text :: LineLexer Clause
    text = ws *> fmap ClauseText restOfLine


parse' :: Parser [Token] Clause
parse' = dotPointClause <|> adhocClause <|> text'

    where
    dotPointClause :: Parser [Token] Clause
    dotPointClause = do
        (col, op) <- junc'
        Conjunction op <$> sepBy1 (matchJunc' col op) parse'

    matchJunc' :: Int -> Op -> Parser [Token] ()
    matchJunc' col op = do
        (col', op') <- junc'
        case (col == col', op == op') of
            (False, _)    -> reject "not aligned"
            (True, False) -> error "mismatch!" -- TODO: would like this reported
            (True, True)  -> pure ()

    adhocClause :: Parser [Token] Clause
    adhocClause = reject "looking for adhoc"

    text' :: Parser [Token] Clause
    text' = Parser f
        where
        f (TText text:ts) = Right (ts, ClauseText text)
        f _               = Left "Not text"

    junc' :: Parser [Token] (Int, Op)
    junc' = Parser f
        where
        f          [] = Left "Out of input"
        f (TOr  c:ts) = Right (ts, (c, Or))
        f (TAnd c:ts) = Right (ts, (c, And))
        f           _ = Left "mismatch"

data Token = TOr !Int
           | TAnd !Int
           | TText !ByteString
               deriving Show

data Chunk =
    Chunk !Int !ByteString
        deriving Show

classify :: Chunk -> [Token]
classify (Chunk col "/\\") = [TAnd col]
classify (Chunk col "\\/") = [TOr col]
classify (Chunk   _    bs) = [TText bs]

lexer :: LineLexer [Token]
lexer = concat <$> many (classify <$> nextChunk) <* ws

nextChunk :: LineLexer Chunk
nextChunk = ws *> (Chunk <$> getCol <*> lTakeWhile1 (not . isSpace))

ws :: LineLexer ()
ws = void (lTakeWhile isSpace)
