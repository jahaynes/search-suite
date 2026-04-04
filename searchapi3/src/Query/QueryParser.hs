{-# LANGUAGE OverloadedStrings #-}

module Query.QueryParser ( Clause (..)
                         , Op (..)
                         , parseQuery ) where

import Parser.Combinators
import Parser.LineLexer
import Parser.Parser

import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Writer  (runWriterT, tell)
import           Control.Applicative         ((<|>))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Char                   (isSpace)
import           Data.Functor                (void)
import           Data.List.NonEmpty          (NonEmpty (..))
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

    case runParser (runWriterT parse) (fromInput bs) of

        Left t ->
            Left [t]

        Right (lexState, (clause, logs))
            | null logs ->
                if C8.null (_input lexState)
                    then Right clause
                    else Left ["Not all input was parsed"]

            -- Prioritise returning feedback to the user,
            -- even (especially) if the overall parse failed
            | otherwise ->
                Left logs

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
            first <- clauseOrText
            rest <- parseRest col op
            pure $ Conjunction op (first :| rest)

            where
            parseRest :: Int -> Op -> LineLexer [Clause]
            parseRest col op =
                (do
                    (col', op') <- junc
                    case (col == col', op == op') of
                        (True, True) -> do
                            c <- clauseOrText
                            cs <- parseRest col op
                            pure (c : cs)
                        (True, False) -> tell ["mismatch!"] *> pure []
                        (False, _)    -> tell ["Junctions not aligned"] *> pure []
                ) <|> pure []

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
