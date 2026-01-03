{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module QueryProcessor ( QueryProcessor (..)
                      , createQueryProcessor
                      ) where

import Component                 ( Component )
import EnvironmentShim           ( getIndexerBinaryImpl )
import QueryParams               ( QueryParams (..) )
import QueryParser               ( Clause (..), Op (..) )
import QueryProcessorTypes       ( SpellingSuggestions (..), QueryResults (..), QueryResult (..), UnscoredResults (..) )
import Registry                  ( Registry (..) )
import Metadata                  ( Metadata (..), MetadataApi (..) )
import Types

import           Control.Concurrent.Async       (forConcurrently)
import           Control.Concurrent.STM         (atomically)
import           Control.DeepSeq                (NFData, deepseq)
import           Control.Exception.Safe         (catchAnyDeep)
import           Control.Monad                  (unless)
import           Data.Aeson                     (FromJSON, eitherDecodeStrict')
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Either                    (partitionEithers)
import           Data.Heap                      (Heap)
import qualified Data.Heap as H
import           Data.List                      (sortOn)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map.Strict as M
import           Data.Maybe                     (fromMaybe)
import qualified Data.Set as S
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import qualified Data.Vector as V
import           GHC.IO.Exception               (ExitCode (..))
import           System.Process.ByteString      (readProcessWithExitCode)
import           Text.Printf                    (printf)
import           UnliftIO.Exception             (bracket)

data QueryProcessor =
    QueryProcessor { runQuery      :: !(CollectionName -> QueryParams -> IO (Either String QueryResults))
                   , runSpelling   :: !(CollectionName -> Text -> Maybe Int -> IO (Either String SpellingSuggestions))
                   , runStructured :: !(CollectionName -> Clause -> IO (Either Text UnscoredResults))
                   }

createQueryProcessor :: Registry
                     -> MetadataApi
                     -> (ByteString -> IO ())
                     -> QueryProcessor
createQueryProcessor reg metadataApi lg =
    QueryProcessor { runQuery      = runQueryImpl reg metadataApi lg
                   , runSpelling   = runSpellingImpl reg lg
                   , runStructured = runStructuredImpl reg lg
                   }

data ComponentResult = 
    ComponentResult QueryResult Component deriving Show

instance Eq ComponentResult where
    (ComponentResult r1 c1) == (ComponentResult r2 c2) = score r1 == score r2 && c1 == c2

instance Ord ComponentResult where
    compare (ComponentResult r1 c1) (ComponentResult r2 c2) = compare (score r1, c1) (score r2, c2)

data Mode = Normal | Regex deriving Eq

-- TODO, this can probably pushed into indexer-qp2
runStructuredImpl :: Registry
                  -> (ByteString -> IO ())
                  -> CollectionName
                  -> Clause
                  -> IO (Either Text UnscoredResults)
runStructuredImpl reg logger collectionName@(CollectionName cn) clause =
    withLocks reg collectionName $ \lockedComponents -> do
        rs <- go lockedComponents clause
        pure . Right $ UnscoredResults (S.size rs) rs

    where
    go lockedComponents (ClauseText q) = do
        Right r <- runUnscored Normal lockedComponents (decodeUtf8 q) -- TODO can this decode be skipped
        pure $ unscored_results r

    go lockedComponents (ClauseRegex q) = do
        Right r <- runUnscored Regex lockedComponents (decodeUtf8 q) -- TODO can this decode be skipped
        pure $ unscored_results r

    go lockedComponents (Conjunction op cs) = do
        let f = case op of
                    Or  -> S.union
                    And -> S.intersection
                    Sub -> S.difference
        r :| rs <- mapM (go lockedComponents) cs
        pure (foldl' f r rs)

    runUnscored :: Mode -> [Component] -> Text -> IO (Either String UnscoredResults)
    runUnscored _ [] _ = do
        let errMsg = printf "No such collection: %s" cn
        logger $ C8.pack errMsg
        pure $ Left errMsg
    
    runUnscored mode lockedComponents q = do

        bin <- getIndexerBinaryImpl

        (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> queryComponent bin logger (execParams lc) (encodeUtf8 q))
        let errMsg = C8.unlines (map C8.pack bads)
        unless (null bads)
               (logger errMsg)
        pure $ if null goods
                then Left $ unlines bads
                else Right $ mconcat goods

        where
        execParams :: Component -> [String]
        execParams component =
            concat [ ["unscored"]
                   , if mode == Regex then ["--mode", "regex"] else []
                   , ["--base_path", path component]
                   ]

withLocks :: NFData a => Registry -> CollectionName -> ([Component] -> IO a) -> IO a
withLocks reg collectionName f =
    bracket acquire release $ \cmps -> do
        y <- f cmps
        y `deepseq` pure y
    where
    acquire = atomically $ do
        components <- S.toList <$> viewCollectionComponents reg collectionName
        mapM_ (takeLock reg) components
        pure components
    release = mapM_ (releaseLockIO reg)

runSpellingImpl :: Registry
                -> (ByteString -> IO ())
                -> CollectionName
                -> Text
                -> Maybe Int
                -> IO (Either String SpellingSuggestions)
runSpellingImpl registry logger collectionName@(CollectionName cn) s mMaxDist = do

    bin <- getIndexerBinaryImpl

    withLocks registry collectionName $ \lockedComponents -> do

        if null lockedComponents

            then do
                let errMsg = printf "No such collection: %s" cn
                logger $ C8.pack errMsg
                pure $ Left errMsg

            else do

                (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> spellingComponent bin lc)

                let errMsg = C8.unlines (map C8.pack bads)
                unless (null bads)
                       (logger errMsg)

                pure $ if null goods
                           then Left $ unlines bads
                           else Right . mconcat . map snd $ goods

    where
    spellingComponent :: FilePath -> Component -> IO (Either String SpellingSuggestions)
    spellingComponent bin cmp =

        let maxDist = fromMaybe 1 mMaxDist

            args = ["spelling", path cmp, show maxDist] ++ (map T.unpack $ T.words s) -- TODO better normalisation

            job = do (exitcode, stdout, stderr) <- readProcessWithExitCode bin args ""
                     case exitcode of
                         ExitSuccess -> pure (SpellingSuggestions <$> eitherDecodeStrict' stdout)
                         _  -> do print $ "stdout was: " <> stdout
                                  pure . Left $ C8.unpack stderr
            handle = pure . Left . show
        in catchAnyDeep job handle

runQueryImpl :: Registry
             -> MetadataApi
             -> (ByteString -> IO ())
             -> CollectionName
             -> QueryParams
             -> IO (Either String QueryResults)
runQueryImpl registry metadataApi logger collectionName@(CollectionName cn) params =

    withLocks registry collectionName $ \lockedComponents -> do

        if null lockedComponents

            then do

                let errMsg = printf "No such collection: %s" cn
                logger $ C8.pack errMsg
                pure $ Left errMsg

            else do

                bin <- getIndexerBinaryImpl

                (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> queryComponent bin logger (execParams lc) (query params))

                let errMsg = C8.unlines (map C8.pack bads)
                unless (null bads)
                       (logger errMsg)

                if null goods
                    then pure . Left $ unlines bads
                    else let merged = mergeQueryResults (maxResults params) goods
                         in Right <$> attachMetadata merged

    where
    execParams :: Component -> [String]
    execParams component =
        concat [ ["query"]
               , case maxResults params of Just n -> [printf "-max_results=%d" n]; Nothing -> []
               , ["--base_path", path component]
               ]

    attachMetadata :: V.Vector (Component, QueryResult) -> IO QueryResults
    attachMetadata vcqrs = do
        xs <- V.forM vcqrs $ \(cmp, qr) -> do
            mmetadata <- lookupMetadata metadataApi (path cmp) (uri qr)
            pure $ qr { metadata = M.delete "uri" . unMetadata <$> mmetadata }
        pure $ QueryResults (V.length xs) xs

    mergeQueryResults :: Maybe Int -> [(Component, QueryResults)] -> V.Vector (Component, QueryResult)
    mergeQueryResults mLimit goods = do

        let limit =
                fromMaybe maxBound mLimit

        -- Gather the [limit] best-scored results into a heap
        let resultHeap :: Heap ComponentResult =
                foldl' (go1 limit) H.empty goods

        -- Since we may have duplicate urls with different scores
        -- gather them into a map
        let resultMap :: M.Map Text (Component, QueryResult) =
                foldl' (\m (ComponentResult qr c) -> M.insertWith (maxBy (score . snd)) (uri qr) (c, qr) m) M.empty resultHeap

            -- Maybe do a foldr or foldmaybe or something to try to simplify+

        -- Could do more with mutable vectors

        -- then resort by score
        let qrs = V.fromList
                . sortOn (negate . score . snd)
                . map snd
                $ M.toList resultMap

        qrs

        where
        maxBy :: Ord b => (a -> b) -> a -> a -> a
        maxBy f a b | f a < f b = b
                    | otherwise = a

        go1 :: Int -> Heap ComponentResult -> (Component, QueryResults) -> H.Heap ComponentResult
        go1 limit h (cmp, QueryResults _ qs) = foldl' go2 h qs

            where
            go2 :: Heap ComponentResult -> QueryResult -> Heap ComponentResult
            go2 h' qr

                -- Not enough results yet
                | H.size h' < limit = H.insert (ComponentResult qr cmp) h'

                | otherwise =

                    case H.viewMin h' of
                        Nothing -> h'
                        Just (ComponentResult hqr _, h'') ->
                            if score qr > score hqr
                                -- Greater than smallest result
                                then H.insert (ComponentResult qr cmp) h''
                                -- Lesser (no change)
                                else h'

queryComponent :: (FromJSON a, NFData a) => FilePath -> (ByteString -> IO ()) -> [String] -> ByteString -> IO (Either String a)
queryComponent bin logger execParams queryStr =

    let job = do logExecution

                 (exitcode, stdout, stderr) <- readProcessWithExitCode bin execParams queryStr

                 case exitcode of

                     ExitSuccess -> do
                         C8.putStr stderr
                         Right <$> decodeOutput stdout

                     _  -> do
                        print $ "stdout was: " <> stdout
                        pure . Left $ C8.unpack stderr

        handle = pure . Left . show

    in catchAnyDeep job handle

    where
    decodeOutput :: FromJSON a => ByteString -> IO a
    decodeOutput stdout =
        case eitherDecodeStrict' stdout of
            Right r -> pure r
            Left _  -> error $ "Could not decode output: " <> take 100 (C8.unpack stdout) <> "..."

    logExecution :: IO ()
    logExecution = logger . C8.pack $ unwords [ "Running binary: "
                                              , show bin
                                              , show execParams
                                              ]
