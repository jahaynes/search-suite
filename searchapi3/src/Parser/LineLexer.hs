{-# LANGUAGE OverloadedStrings #-}

module Parser.LineLexer where

import Parser.Parser (Parser (..))

import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Writer  (WriterT)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Text                   (Text)
import qualified Data.Text as T
import           Data.Text.Encoding          (decodeUtf8)

data LexState =
    LexState { _row   :: !Int
             , _col   :: !Int
             , _input :: !ByteString
             }

fromInput :: ByteString -> LexState
fromInput = LexState 0 0

type LineLexer a =
    WriterT [Text] (Parser LexState) a

getCol :: LineLexer Int
getCol = lift $ Parser (\ls@(LexState _ c _ ) -> Right (ls, c))

lexChar :: (Char -> Bool) -> LineLexer Char
lexChar = lift . lexChar'

lexChar' :: (Char -> Bool) -> Parser LexState Char
lexChar' f = Parser go
    where
    go (LexState r c i)

        | C8.null i =
            Left "Insufficient input"

        | f first = let (r', c') = newLineCol r c start in
            Right (LexState r' c' rest, first)

        | otherwise =
            Left $ "Unsatisfied char f(" <> T.pack (show first) <> ")"

        where
        first         = C8.head i
        (start, rest) = C8.splitAt 1 i

lexString :: ByteString -> LineLexer ByteString
lexString = lift . lexString'

lexString' :: ByteString -> Parser LexState ByteString
lexString' bs = Parser go

    where
    go (LexState r c i)

        | bsLength > inputLength =
            Left "Insufficient input"

        | start == bs = let (r', c') = newLineCol r c bs in
            Right (LexState r' c' rest, bs)

        | otherwise =
            Left $ "Expected: " <> decodeUtf8 bs -- TODO not too happy about this decode

        where
        bsLength      = C8.length bs
        inputLength   = C8.length i
        (start, rest) = C8.splitAt bsLength i

lTakeWhile :: (Char -> Bool) -> LineLexer ByteString
lTakeWhile = lift . lTakeWhile'

lTakeWhile' :: (Char -> Bool) -> Parser LexState ByteString
lTakeWhile' p = Parser go
    where
    go (LexState r c i) =
        let (some, rest) = C8.span p i
            (r', c')     = newLineCol r c some
        in Right (LexState r' c' rest, some)

restOfLine :: LineLexer ByteString
restOfLine = lift $ Parser go

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
