{-# LANGUAGE LambdaCase
           , OverloadedStrings #-}

module QueryProcessor ( QueryProcessor (..)
                      , createQueryProcessor
                      , runQueryImpl ) where

import Component           ( Component )
import Data.Warc.WarcEntry ( WarcEntry (..), decompress)
import Data.Warc.Body
import Environment         ( Environment (..) )
import QueryParams         ( QueryParams (..) )
import QueryProcessorTypes
import Registry            ( Registry (..) )
import Types
import WarcFileReader

import           Control.Concurrent.Async       (async, wait)
import           Control.Concurrent.STM         (atomically)
import           Control.Exception.Safe         (catchAnyDeep)
import           Control.Monad                  (forM, unless)
import           Data.Aeson                     (eitherDecodeStrict')
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8    as C8 (pack, unlines, unpack)
import           Data.Either                    (partitionEithers)
import           Data.List                      (sortOn)
import qualified Data.Map.Strict                (Map)
import qualified Data.Map.Strict as M
import           Data.Ord                       (comparing)
import           Data.Set                       (toList)
import           Data.Text                      (Text)
import           Data.Text.Encoding
import qualified Data.Vector              as V
import           GHC.IO.Exception               (ExitCode (..))
import           System.Process.ByteString      (readProcessWithExitCode)
import           Text.HTML.TagSoup
import           Text.Printf                    (printf)

newtype QueryProcessor =
    QueryProcessor { runQuery :: CollectionName -> QueryParams -> IO (Either String QueryResults)
                   }

createQueryProcessor :: Environment
                     -> Registry
                     -> WarcFileReader
                     -> (ByteString -> IO ())
                     -> QueryProcessor
createQueryProcessor env reg wfr lg =
    QueryProcessor { runQuery = runQueryImpl env reg wfr lg
                   }

runQueryImpl :: Environment
             -> Registry
             -> WarcFileReader
             -> (ByteString -> IO ())
             -> CollectionName
             -> QueryParams
             -> IO (Either String QueryResults)
runQueryImpl env registry wfr logger collectionName@(CollectionName cn) params = do

    lockedComponents <- atomically $ do
        components <- toList <$> viewCollectionComponents registry collectionName
        mapM_ (takeLock registry) components
        pure components

    if null lockedComponents
        then do
            let errMsg = printf "No such collection: %s" cn
            logger $ C8.pack errMsg
            pure $ Left errMsg

        else do
            as <- forM lockedComponents $ async . queryAndUnlockComponent
            (bads, goods) <- partitionEithers <$> mapM wait as
            let errMsg = C8.unlines (map C8.pack bads)
            unless (null bads)
                   (logger errMsg)

            pure $ if null goods
                    then Left $ unlines bads
                    else Right $ limit (maxResults params) (mergeQueryResults goods)

    where
    mergeQueryResults :: [QueryResults] -> QueryResults
    mergeQueryResults goods = do

        let qrs = concatMap (V.toList . results) goods

        -- TODO snippet should not exist at this stage here
        -- TODO better merge strat?
        let m = M.fromListWith (\qra qrb -> qra) . map (\(QueryResult u s t n) -> (u, (s, t, n))) $ qrs

        QueryResults { num_results = M.size m
                     , results     = V.fromList
                                   . sortOn (negate . score)
                                   . map (\(u, (s, t, n)) -> QueryResult u s t n)
                                   . M.toList
                                   $ m
                     }

    queryAndUnlockComponent :: Component -> IO (Either String QueryResults)
    queryAndUnlockComponent component =

        let job = do logExecution

                     (exitcode, stdout, stderr) <- readProcessWithExitCode (indexerBinary env) execParams (query params)

                     result <- case exitcode of

                                   ExitSuccess -> do
                                       decodeOut <- decodeOutput stdout
                                       addSnippets component decodeOut

                                   _  -> do print $ "stdout was: " <> stdout
                                            pure . Left $ C8.unpack stderr

                     releaseLockIO registry component

                     pure result

            handle ioe = do releaseLockIO registry component
                            pure $ Left (show ioe)

        in catchAnyDeep job handle

        where
        decodeOutput :: ByteString -> IO QueryResults
        decodeOutput stdout =
            case eitherDecodeStrict' stdout of
                Right r -> pure r
                Left l  -> error $ "Could not decode output: " <> take 100 (C8.unpack stdout) <> "..."

        logExecution :: IO ()
        logExecution = logger . C8.pack $ unwords [ "Running binary: "
                                                  , show (indexerBinary env)
                                                  , show execParams
                                                  ]

        execParams :: [String]
        execParams = concat [ ["query"]
                            , case maxResults params of Just n -> [printf "-max_results=%d" n]; Nothing -> []
                            , [path component]
                            ]

    -- Make this happen after the max_results pruning
    -- TODO break out snippets code as own module
    -- TOO SLOW (approx all the work!)
    addSnippets :: Component -> QueryResults -> IO (Either String QueryResults)
    addSnippets component (QueryResults n qrs) = do

        snippets <- V.mapM getSnippet qrs
        let qrs' = V.zipWith (\qr mt -> qr {snippet = mt}) qrs snippets
        pure . Right $ QueryResults n qrs'

        where
        getSnippet :: QueryResult -> IO (Maybe Text)
        getSnippet qr = do

            let destWarcFile = path component <> "/" <> "file.warc"
                destOffsets  = path component <> "/" <> "file.offs"
                url          = encodeUtf8 $ uri qr

            (fmap decompress <$> findWarcEntry wfr destWarcFile destOffsets url) >>= \case
                Nothing -> pure Nothing -- Should always be a Just here
                Just (WarcEntry _ (UncompressedBody body)) -> pure $ summarise body

        summarise :: ByteString -> Maybe Text
        summarise body = do

            let desc = map snd
                     . concatMap (\(TagOpen "meta" attrs) -> filter (\(k,_) -> k == "content" ) attrs)
                     . filter (\(TagOpen "meta" attrs) -> any (==("name", "description")) attrs)
                     . filter (isTagOpenName "meta")
                     . takeWhile (not . isTagOpenName "body")
                     . parseTags
                     $ body

            case desc of
                []    -> Just "No description"
                (x:_) -> case decodeUtf8' x of 
                             Left l ->  Nothing
                             Right r -> Just r
