{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric,
             OverloadedStrings,
             LambdaCase,
             ScopedTypeVariables #-}

module Main where

import           Data.Warc.Body
import           Data.Warc.Header
import           Data.Warc.Key
import           Data.Warc.Parse
import           Data.Warc.Value
import           Data.Warc.WarcEntry         (WarcEntry (..))
import qualified Data.Warc.WarcEntry as W

import           Control.DeepSeq             (deepseq)
import           Control.Monad               (forM_)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Either                 (rights)
import           Data.List                   (groupBy)
import           Data.List.Split             (chunksOf, splitOn)
import           Data.Ord                    -- (comparing)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8')
import           GHC.Generics                (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Types          (statusCode)
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

    let entries :: [WarcEntry] = rights (fromByteString content)   
    
    let grouped :: [[WarcEntry]] = chunksOf (4 * 10) entries

    forM_ grouped $ \group ->
        deepseq (group :: [WarcEntry]) $ do
            
            let xs :: [(Int, WarcEntry)] = zip [0..] group

            let ys :: [[(Int, WarcEntry)]] = groupBy (\a b -> fst a `mod` 4 == fst b `mod` 4) xs

            let zs = map (map (\(a,b) -> (a,"we"))) ys

            error $ show zs


    hClose h
    pure ()

-- postEntry man collection c

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
                            } ) <$> parseRequest (printf "http://127.0.0.1:8081/indexDoc/%s" collection)
            resp <- httpNoBody req man
            let code = statusCode (responseStatus resp)
            printf "%d %s\n" code (d_url doc)

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
