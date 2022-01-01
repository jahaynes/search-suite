{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             OverloadedStrings,
             LambdaCase #-}

module Main where

import           Data.Warc.Body
import           Data.Warc.Header
import           Data.Warc.Key
import           Data.Warc.Parse
import           Data.Warc.Value
import           Data.Warc.WarcEntry         (WarcEntry (..))
import qualified Data.Warc.WarcEntry as W

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Either                 (rights)
import           Data.List.Split             (splitOn)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8')
import           GHC.Generics                (Generic)
import           Network.HTTP.Client
import           System.Environment          (getArgs)
import           System.IO
import           Text.Printf

newtype IndexRequest =
    IndexRequest { docs :: [Doc] }
        deriving (Generic, ToJSON)

data Doc = 
    Doc { d_url     :: !Text
        , d_content :: !Text
        } deriving Generic

instance ToJSON Doc where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = concat . drop 1 . splitOn "_"
                                          , unwrapUnaryRecords = True
                                          }

main :: IO ()
main = do

    getArgs >>= \case

        (collection:warcFiles) -> do
            man <- newManager defaultManagerSettings
            mapM_ (ingest man collection) warcFiles

        _ -> error "Usage: ./warcCrawler collection warcFile1 warcFile2 ..."

ingest :: Manager -> String -> FilePath -> IO ()    -- TODO exceptions/resources
ingest man collection filePath = do
    h <- openFile filePath ReadMode
    content <- LBS.hGetContents h
    let entries = rights (fromByteString content)
    mapM_ (postEntry man collection) entries
    hClose h
    pure ()

postEntry :: Manager -> String -> WarcEntry -> IO ()
postEntry man collection entry =
    case toDoc entry of
        Left e    -> putStrLn e
        Right doc -> do
            let payload = encode $ IndexRequest [ doc ]
            req <- (\r -> r { method          = "POST"
                            , requestHeaders  = [("Content-Type", "application/json")]
                            , requestBody     = RequestBodyLBS payload
                            , responseTimeout = responseTimeoutNone -- responseTimeoutMicro 10000000
                            } ) <$> parseRequest (printf "http://127.0.0.1:8081/index/%s" collection)
            print =<< httpNoBody req man

toDoc :: WarcEntry -> Either String Doc
toDoc entry@(WarcEntry _ CompressedBody{}) = toDoc $ W.decompress entry
toDoc (WarcEntry header (UncompressedBody body)) =
    Doc <$> getTargetUri <*> getBody

    where
    getTargetUri :: Either String Text
    getTargetUri =
        case getValue (OptionalKey WarcTargetURI) header of
            Nothing -> Left "No WARC-Target-URI in entry"
            Just (StringValue uri) ->
                case decodeUtf8' uri of
                    Left _        -> Left "Could not decode WARC-Target-URI in entry"
                    Right utf8Uri -> Right utf8Uri
            _ -> error $ "Unexpected value type in WARC-Target-URI!"

    getBody :: Either String Text
    getBody =
        case decodeUtf8' body of
            Left _         -> Left "Could not decode entry body"
            Right utf8Body -> Right utf8Body
