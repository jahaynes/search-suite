{-# LANGUAGE OverloadedStrings #-}

module Parser.LineLexer where

import Parser.Parser (Parser (..))

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8

data LexState =
    LexState { _row   :: !Int
             , _col   :: !Int
             , _input :: !ByteString
             }

fromInput :: ByteString -> LexState
fromInput = LexState 0 0

type LineLexer a =
    Parser LexState a

getCol :: LineLexer Int
getCol = Parser (\ls@(LexState _ c _ ) -> Right (ls, c))

lexChar :: (Char -> Bool) -> LineLexer Char
lexChar f = Parser go
    where
    go (LexState r c i)

        | C8.null i =
            Left "Insufficient input"

        | f first = let (r', c') = newLineCol r c start in
            Right (LexState r' c' rest, first)

        | otherwise =
            Left $ "Unsatisfied char f(" <> C8.pack (show first) <> ")"

        where
        first         = C8.head i
        (start, rest) = C8.splitAt 1 i

lexString :: ByteString -> LineLexer ByteString
lexString bs = Parser go

    where
    go (LexState r c i)

        | bsLength > inputLength =
            Left "Insufficient input"

        | start == bs = let (r', c') = newLineCol r c bs in
            Right (LexState r' c' rest, bs)

        | otherwise =
            Left $ "Expected: " <> bs

        where
        bsLength      = C8.length bs
        inputLength   = C8.length i
        (start, rest) = C8.splitAt bsLength i

lTakeWhile :: (Char -> Bool) -> LineLexer ByteString
lTakeWhile p = Parser go
    where
    go (LexState r c i) =
        let (some, rest) = C8.span p i
            (r', c')     = newLineCol r c some
        in Right (LexState r' c' rest, some)

restOfLine :: LineLexer ByteString
restOfLine = Parser go

    where
    go (LexState r c i) =
        let (start, rest) = C8.break (=='\n') i
            (r', c')      = newLineCol r c start
        in Right (LexState r' c' rest, start)

newLineCol :: Int -> Int -> ByteString -> (Int, Int)
newLineCol r c consumed =

    let len = C8.length consumed in

    case C8.elemIndices '\n' consumed of

        -- No newlines hit.  We only moved right
        [] -> (r, c + len)

        -- Some newlines hit.  Adjust both row and col
        ns -> (r + length ns, len - last ns - 1)
