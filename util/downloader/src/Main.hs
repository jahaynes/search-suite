module Main where

import Data.Maybe         (mapMaybe)
import Data.Set           (Set, fromList, member)
import System.Directory   (listDirectory)
import System.Environment (getArgs)
import Text.Printf        (printf)

basePath :: String
basePath = "https://commoncrawl.s3.amazonaws.com/"

main :: IO ()
main = do

    [ddir, pathsFile] <- getArgs

    downloaded <- fromList <$> listDirectory ddir

    paths <- readFile pathsFile

    mapM_ putStrLn . mapMaybe (go downloaded)
                   . lines
                   $ paths

go :: Set FilePath -> String -> Maybe String
go downloaded line = do

   let simpleFileName = reverse
                      . takeWhile (/= '/')
                      . drop 3
                      . reverse
                      $ line

   if simpleFileName `member` downloaded
       then Nothing
       else Just $
           printf "curl %s%s | gunzip | ./comp -r -c > %s" basePath line simpleFileName

   
