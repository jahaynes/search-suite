{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Warc.Header                 (getValue)
import           Data.Warc.Key                    (Key (MandatoryKey), MandatoryKey (WarcType))
import           Data.Warc.Parse                  (fromByteString)
import           Data.Warc.Value                  (Value (..))
import           Data.Warc.WarcEntry              (WarcEntry (..), compress, decompress, toLazyByteString)

import qualified Data.ByteString.Lazy.Char8 as L8 (getContents, putStr)
import           Data.Either                      (fromRight)
import           Data.List                        (partition)
import           Data.Maybe                       (mapMaybe)
import           System.Environment               (getArgs)

data Compressed = Compressed
                | Uncompressed

data Entries = All
             | Responses

getOpts :: IO (Compressed, Entries)
getOpts = do
    (goodArgs, badArgs) <- partition (\a -> a == "-c" || a == "-r") <$> getArgs
    case badArgs of
        [] -> do let compressed = if "-c" `elem` goodArgs then Compressed else Uncompressed
                 let entries    = if "-r" `elem` goodArgs then  Responses else All
                 pure (compressed, entries)
        ba -> error $ "Bad arguments: " ++ unwords ba

main :: IO ()
main = do
    (compressed, entries) <- getOpts
    process compressed entries

process :: Compressed -> Entries -> IO ()
process compressed entries = 
    mapM_ (L8.putStr . toLazyByteString . compression)
          . mapMaybe (keep . fromRight (error "bad"))
          . fromByteString
        =<< L8.getContents

    where
    compression :: WarcEntry -> WarcEntry
    compression =
      case compressed of
        Compressed   -> compress
        Uncompressed -> decompress

    keep :: WarcEntry -> Maybe WarcEntry
    keep we@(WarcEntry h _) =
      case entries of
        All       -> Just we
        Responses -> 
          case getValue (MandatoryKey WarcType) h of
            Just (StringValue "response") -> Just we
            _                             -> Nothing
