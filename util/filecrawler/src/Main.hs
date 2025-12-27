{-# LANGUAGE LambdaCase,
             OverloadedStrings,
             ScopedTypeVariables #-}

module Main where

import Types

import           Control.Concurrent.Async           (forConcurrently_, mapConcurrently)
import           Control.Exception.Safe             (tryAny)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Data.Aeson                         (decode, encode)
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy.Char8   as L8
import           Data.Char                          (isAscii, toLower)
import           Data.Conduit                       ((.|), runConduitRes)
import qualified Data.Conduit.List            as CL
import           Data.Conduit.Combinators           (sourceDirectoryDeep)
import           Data.List                          (isPrefixOf)
import           Data.Maybe                         (catMaybes)
import           Data.Text as T                     (Text, pack)
import           Data.Text.Encoding                 (decodeUtf8')
import           Network.HostName                   (getHostName)
import           Network.HTTP.Types.Header          (hContentType)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status          (statusCode)
import           System.Environment                 (getArgs)
import           Text.Printf                        (printf)

main :: IO ()
main = do

    hostname   <- getHostName
    http       <- newManager defaultManagerSettings
    -- To prevent gathering from the same directory the indexer is trying to write
    noIndexDir <- fetchCollectionDir http

    [colName, startPath] <- getArgs
    let followSymlinks = False
    runConduitRes $ sourceDirectoryDeep followSymlinks startPath
                .| CL.filter (\p -> not (noIndexDir `isPrefixOf` p))
                .| CL.chunksOf 20
                .| CL.mapM (liftIO . mapConcurrently (processFile http))
                .| CL.map catMaybes
                .| CL.map (map (\(fp, _content) -> (replaceNonAscii fp, _content)))
                .| CL.mapM_ (send http hostname colName)

fetchCollectionDir :: Manager -> IO FilePath
fetchCollectionDir http = do
    request  <- parseRequest "http://127.0.0.1:8081/collection-dir"
    response <- httpLbs request http
    case statusCode (responseStatus response) of
        200 -> let Just x = decode (responseBody response) in pure x
        _   -> error $ show response

replaceNonAscii :: FilePath -> FilePath
replaceNonAscii = map go
    where
    go x | isAscii x = x
         | otherwise = '?'

processFile :: Manager -> FilePath -> IO (Maybe (FilePath, Text))
processFile http filePath
    | any (`endsWith` filePath) richFiletypes      = processTika http filePath
    | any (`endsWith` filePath) plaintextFileTypes = processPlainText filePath
    | otherwise                                    = pure Nothing

    where
    endsWith :: String -> String -> Bool
    endsWith ext fp = map toLower ext == (reverse . map toLower . take (length ext) . reverse $ fp)

    plaintextFileTypes :: [String]
    plaintextFileTypes = ["cs", "js", "ts", "tsx", "sql"]

    richFiletypes :: [String]
    richFiletypes = []

processTika :: Manager -> FilePath -> IO (Maybe (FilePath, Text))
processTika http filePath = do
    initialRequest <- parseRequest "http://127.0.0.1:8080/extract"
    let request = initialRequest { method = "POST"
                                 , requestBody = RequestBodyLBS $ encode (Filepath filePath) 
                                 }
    tryAny (httpLbs request http) >>= \case
        Left{} -> do
            putStrLn $ "Could not reach rich text extractor for: " ++ filePath
            pure Nothing
        Right response ->
            let sc = statusCode $ responseStatus response in
            case sc of
                200 -> case decodeUtf8' . L8.toStrict . responseBody $ response of
                    Left _ -> do
                        putStrLn $ "Bad UTF-8 in from Tika: " <> filePath
                        pure Nothing
                    Right txt ->
                        pure $ Just (filePath, txt)
                _ -> do
                    putStrLn $ "Unexpected status code: " ++ show sc
                    pure Nothing

processPlainText :: FilePath -> IO (Maybe (FilePath, Text))
processPlainText filePath = do
    cont <- C8.readFile filePath
    case decodeUtf8' cont of
        Left _ -> do putStrLn $ "Bad UTF-8 in plaintextfile: " <> filePath
                     pure Nothing
        Right txt -> pure . Just $ (filePath, txt)

send :: MonadIO m => Manager -> String -> String -> [(FilePath, Text)] -> m ()
send http fromHostName colName filePathsAndBodies = liftIO $ do

  forConcurrently_ filePathsAndBodies $ \(filePath, body) -> do

      let doc = Doc { url     = T.pack $ mconcat ["file://", fromHostName, filePath]
                    , content = body
                    }

      let ir = IndexRequest [doc]

      initialRequest <- parseRequest $ printf "http://127.0.0.1:8081/indexDocs/%s" colName
      let request = initialRequest { method = "POST"
                                   , requestHeaders = (hContentType, "application/json") : requestHeaders initialRequest
                                   , requestBody = RequestBodyLBS $ encode ir }
      resp <- httpLbs request http
      putStrLn $ show (statusCode $ responseStatus resp) ++ ": " ++ filePath
