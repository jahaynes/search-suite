{-# LANGUAGE OverloadedStrings #-}

module Parser.LineLexer where

import Parser.Transformer (ParserT (..))

import           Data.Functor.Identity       (Identity (..))
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

data LexState =
    LexState { _row   :: !Int
             , _col   :: !Int
             , _input :: !ByteString
             }

fromInput :: ByteString -> LexState
fromInput = LexState 0 0

type LineLexer a =
    ParserT LexState Identity a

getCol :: LineLexer Int
getCol = ParserT (\ls@(LexState _ c _ ) -> Identity (Right (ls, c)))

lexChar :: (Char -> Bool) -> LineLexer Char
lexChar f = ParserT go
    where
    go (LexState r c i)

        | C8.null i =
            Identity (Left "Insufficient input")

        | f first = let (r', c') = newLineCol r c start in
            Identity (Right (LexState r' c' rest, first))

        | otherwise =
            Identity (Left $ "Unsatisfied char f(" <> T.pack (show first) <> ")")

        where
        first         = C8.head i
        (start, rest) = C8.splitAt 1 i

lexString :: ByteString -> LineLexer ByteString
lexString bs = ParserT go

    where
    go (LexState r c i)

        | bsLength > inputLength =
            Identity (Left "Insufficient input")

        | start == bs = let (r', c') = newLineCol r c bs in
            Identity (Right (LexState r' c' rest, bs))

        | otherwise =
            Identity (Left $ "Expected: " <> decodeUtf8 bs) -- TODO not too happy about this decode

        where
        bsLength      = C8.length bs
        inputLength   = C8.length i
        (start, rest) = C8.splitAt bsLength i

lTakeWhile :: (Char -> Bool) -> LineLexer ByteString
lTakeWhile p = ParserT go
    where
    go (LexState r c i) =
        let (some, rest) = C8.span p i
            (r', c')     = newLineCol r c some
        in Identity (Right (LexState r' c' rest, some))

restOfLine :: LineLexer ByteString
restOfLine = ParserT go

    where
    go (LexState r c i) =
        let (start, rest) = C8.break (=='\n') i
            (r', c')      = newLineCol r c start
        in Identity (Right (LexState r' c' rest, start))

newLineCol :: Int -> Int -> ByteString -> (Int, Int)
newLineCol r c consumed =

    let len = C8.length consumed in

    case C8.elemIndices '\n' consumed of

        -- No newlines hit.  We only moved right
        [] -> (r, c + len)

        -- Some newlines hit.  Adjust both row and col
        ns -> (r + length ns, len - last ns - 1)