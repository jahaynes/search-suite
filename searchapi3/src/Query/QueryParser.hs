{-# LANGUAGE OverloadedStrings #-}

module Query.QueryParser ( Clause (..)
                         , Op (..)
                         , parseQuery ) where

import Parser.Combinators
import Parser.LineLexer

import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Writer  (tell)
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

parseQuery :: ByteString -> Either [Text] Clause
parseQuery bs =
    runLineLexer parse (fromInput bs) >>= \(ls, p) ->
        case C8.null (_input ls) of
            True  -> Right p
            False -> Left ["Parse failure (leftover)"]

-- Still to do.  Check AND-OR mismatches,  AND/ORs introduced out of nowhere
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
                    (False, _)    -> lift (reject "not aligned")
                    (True, False) -> error "TOLD" -- tell ["mismatch!"] *> lift (reject "mismatch!")
                    (True, True)  -> pure ()

        junc :: LineLexer (Int, Op)
        junc = ws *> ((,) <$> getCol <*> parseOp)
            where
            parseOp = (Or  <$ lexString "\\/")
                  <|> (And <$ lexString "/\\")
                  <|> (Sub <$ lexString "--")

    regex :: LineLexer Clause
    regex = ws
         *> lexString "~"
         *> ws
         *> fmap ClauseRegex restOfLine

    text :: LineLexer Clause
    text = ws *> fmap ClauseText restOfLine

    ws :: LineLexer ()
    ws = void (lTakeWhile isSpace)
