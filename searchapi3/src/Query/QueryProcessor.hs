{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module Query.QueryProcessor ( QueryProcessor (..)
                            , createQueryProcessor
                            ) where

import Component                 ( Component )
import Environment               ( Environment (..) )
import Query.QueryParams         ( QueryParams (..) )
import Query.QueryProcessorTypes ( SpellingSuggestions (..), QueryResults (..), QueryResult (..) )
import Registry                  ( Registry (..) )
import Metadata                  ( Metadata (..), MetadataApi (..) )
import Types

import           Control.Concurrent.Async       (forConcurrently)
import           Control.Concurrent.STM         (atomically)
import           Control.DeepSeq                (NFData, deepseq)
import           Control.Exception.Safe         (catchAnyDeep)
import           Control.Monad                  (unless)
import           Data.Aeson                     (eitherDecodeStrict')
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Either                    (partitionEithers)
import           Data.Heap                      (Heap)
import qualified Data.Heap as H
import           Data.List                      (sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe                     (fromMaybe)
import           Data.Set                       (toList)
import           Data.Text                      (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.IO.Exception               (ExitCode (..))
import           System.Process.ByteString      (readProcessWithExitCode)
import           Text.Printf                    (printf)
import           UnliftIO.Exception             (bracket)

data QueryProcessor =
    QueryProcessor { runSpelling :: !(CollectionName -> Text -> Maybe Int -> IO (Either String SpellingSuggestions))
                   , runQuery    :: !(CollectionName -> QueryParams -> IO (Either String QueryResults))
                   }

createQueryProcessor :: Environment
                     -> Registry
                     -> MetadataApi
                     -> (ByteString -> IO ())
                     -> QueryProcessor
createQueryProcessor env reg metadataApi lg =
    QueryProcessor { runSpelling = runSpellingImpl env reg lg
                   , runQuery    = runQueryImpl env reg metadataApi lg
                   }

data ComponentResult = 
    ComponentResult QueryResult Component deriving Show

instance Eq ComponentResult where
    (ComponentResult r1 c1) == (ComponentResult r2 c2) = score r1 == score r2 && c1 == c2

instance Ord ComponentResult where
    compare (ComponentResult r1 c1) (ComponentResult r2 c2) = compare (score r1, c1) (score r2, c2)

withLocks :: NFData a => Registry -> CollectionName -> ([Component] -> IO a) -> IO a
withLocks reg collectionName f =
    bracket acquire release $ \cmps -> do
        y <- f cmps
        y `deepseq` pure y
    where
    acquire = atomically $ do
        components <- toList <$> viewCollectionComponents reg collectionName
        mapM_ (takeLock reg) components
        pure components
    release = mapM_ (releaseLockIO reg)

runSpellingImpl :: Environment
                -> Registry
                -> (ByteString -> IO ())
                -> CollectionName
                -> Text
                -> Maybe Int
                -> IO (Either String SpellingSuggestions)
runSpellingImpl env registry logger collectionName@(CollectionName cn) s mMaxDist =

    withLocks registry collectionName $ \lockedComponents -> do

        if null lockedComponents

            then do
                let errMsg = printf "No such collection: %s" cn
                logger $ C8.pack errMsg
                pure $ Left errMsg

            else do

                (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> spellingComponent lc)

                let errMsg = C8.unlines (map C8.pack bads)
                unless (null bads)
                       (logger errMsg)

                pure $ if null goods
                           then Left $ unlines bads
                           else Right . mconcat . map snd $ goods

    where
    spellingComponent :: Component -> IO (Either String SpellingSuggestions)
    spellingComponent cmp =

        let maxDist = fromMaybe 1 mMaxDist

            args = ["spelling", path cmp, show maxDist] ++ (map T.unpack $ T.words s) -- TODO better normalisation

            job = do (exitcode, stdout, stderr) <- readProcessWithExitCode (indexerBinary env) args ""
                     case exitcode of
                         ExitSuccess -> pure (SpellingSuggestions <$> eitherDecodeStrict' stdout)
                         _  -> do print $ "stdout was: " <> stdout
                                  pure . Left $ C8.unpack stderr
            handle = pure . Left . show
        in catchAnyDeep job handle

runQueryImpl :: Environment
             -> Registry
             -> MetadataApi
             -> (ByteString -> IO ())
             -> CollectionName
             -> QueryParams
             -> IO (Either String QueryResults)
runQueryImpl env registry metadataApi logger collectionName@(CollectionName cn) params =

    withLocks registry collectionName $ \lockedComponents -> do

        if null lockedComponents

            then do

                let errMsg = printf "No such collection: %s" cn
                logger $ C8.pack errMsg
                pure $ Left errMsg

            else do

                (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> queryComponent lc)

                let errMsg = C8.unlines (map C8.pack bads)
                unless (null bads)
                       (logger errMsg)

                if null goods
                    then pure . Left $ unlines bads
                    else let merged = mergeQueryResults (maxResults params) goods
                         in Right <$> attachMetadata merged

    where
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
                    let Just (ComponentResult hqr _, h'') = H.viewMin h'
                    in if score qr > score hqr

                           -- Greater than smallest result
                           then H.insert (ComponentResult qr cmp) h''

                           -- Lesser (no change)
                           else h'

    queryComponent :: Component -> IO (Either String QueryResults)
    queryComponent component =

        let job = do logExecution

                     (exitcode, stdout, stderr) <- readProcessWithExitCode (indexerBinary env) execParams (query params)

                     case exitcode of

                         ExitSuccess -> Right <$> decodeOutput stdout

                         _  -> do print $ "stdout was: " <> stdout
                                  pure . Left $ C8.unpack stderr

            handle = pure . Left . show

        in catchAnyDeep job handle

        where
        decodeOutput :: ByteString -> IO QueryResults
        decodeOutput stdout =
            case eitherDecodeStrict' stdout of
                Right r -> pure r
                Left _  -> error $ "Could not decode output: " <> take 100 (C8.unpack stdout) <> "..."

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
