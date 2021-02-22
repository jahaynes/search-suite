{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             OverloadedStrings #-}

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
    (collection:warcFiles) <- getArgs
    man <- newManager defaultManagerSettings
    mapM_ (ingest man collection) warcFiles

ingest :: Manager -> String -> FilePath -> IO ()
ingest man collection filePath = do
    h <- openFile filePath ReadMode
    content <- LBS.hGetContents h
    let entries = rights (fromByteString content)
    mapM_ (postEntry man collection) entries
    hClose h
    pure ()

postEntry :: Manager -> String -> WarcEntry -> IO ()
postEntry man collection entry = do

    let WarcEntry header (UncompressedBody body) = W.decompress entry

    let Just (StringValue uri) = getValue (OptionalKey WarcTargetURI) header

    case decodeUtf8' body of

        Left _ -> pure ()

        Right utf8Body -> do

            case decodeUtf8' uri of

                Left _ -> pure ()

                Right utf8Uri -> do

                    let payload = encode $ IndexRequest [ Doc { d_url     = utf8Uri
                                                              , d_content = utf8Body
                                                              }
                                                        ]

                    req <- (\r -> r { method          = "POST"
		                    , requestHeaders  = [("Content-Type", "application/json")]
                                    , requestBody     = RequestBodyLBS payload
                                    , responseTimeout = responseTimeoutNone -- responseTimeoutMicro 10000000
                                    } ) <$> parseRequest (printf "http://127.0.0.1:8081/index/%s" collection)

                    print =<< httpNoBody req man
