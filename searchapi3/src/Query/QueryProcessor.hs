{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , ScopedTypeVariables #-}

module Query.QueryProcessor ( QueryProcessor (..)
                            , createQueryProcessor
                            ) where

import Component                 ( Component )
import Environment               ( Environment (..) )
import Logger                    ( Logger (..) )
import Metadata                  ( Metadata (..), MetadataApi (..) )
import Query.QueryCommon         ( queryComponent, withLocks )
import Query.QueryParams         ( QueryParams (..) )
import Query.QueryProcessorTypes ( QueryResults (..), QueryResult (..) )
import Registry                  ( Registry (..) )
import Types

import           Control.Concurrent.Async   (forConcurrently)
import           Control.Monad              (unless)
import           Data.Either                (partitionEithers)
import qualified Data.Map.Strict as M
import           Data.Maybe                 (fromMaybe)
import           Data.Ord                   (Down (..))
import           Data.String.Interpolate    (i)
import           Data.Text                  (Text)
import qualified Data.Vector as V
import           Data.List                  (sortOn)

newtype QueryProcessor =
    QueryProcessor { runQuery :: CollectionName -> QueryParams -> IO (Either [Text] QueryResults)
                   }

createQueryProcessor :: Environment
                     -> Registry
                     -> MetadataApi
                     -> Logger
                     -> QueryProcessor
createQueryProcessor env reg metadataApi lg =
    QueryProcessor { runQuery = runQueryImpl env reg metadataApi lg
                   }

runQueryImpl :: Environment
             -> Registry
             -> MetadataApi
             -> Logger
             -> CollectionName
             -> QueryParams
             -> IO (Either [Text] QueryResults)
runQueryImpl env registry metadataApi logger collectionName@(CollectionName cn) params =

    withLocks registry collectionName $ \lockedComponents ->

        if null lockedComponents

            then
                pure $ Left [[i|No such collection: #{cn}|]]

            else do

                (badss, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> queryComponent env logger (execParams lc) (query params))

                let bads = concat badss

                unless (null bads)
                       (info logger bads)

                if null goods
                    then pure $ Left bads
                    else let merged = mergeQueryResults (maxResults params) goods
                         in Right <$> attachMetadata merged

    where
    execParams :: Component -> [String]
    execParams component =
        concat [ ["query"]
               , case maxResults params of Just n -> ["-max_results=" ++ show n]; Nothing -> []
               , ["--base_path", path component]
               ]

    -- Batch metadata lookups per component (avoids O(N) file open/close),
    -- then parallelise across components.
    attachMetadata :: V.Vector (Component, QueryResult) -> IO QueryResults
    attachMetadata vcqrs = do
        -- Group by component path for batched file-handle reuse
        let grouped :: M.Map FilePath [(Int, Component, QueryResult)]
            grouped = V.ifoldl' (\m i (cmp, qr) -> M.insertWith (++) (path cmp) [(i, cmp, qr)] m) M.empty vcqrs

        -- Look up metadata in parallel across components, batched within each
        resultsList <- forConcurrently (M.toList grouped) $ \(fp, indexedPairs) -> do
            let uris = map (\(_, _, qr) -> uri qr) indexedPairs
            mmetas <- lookupMetadataBatch metadataApi fp uris
            pure $ zipWith (\(idx, _, qr) mmeta -> (idx, qr { metadata = M.delete "uri" . unMetadata <$> mmeta })) indexedPairs mmetas

        -- Reassemble in original order (already sorted by score from merge)
        let xs = V.fromList . map snd . sortOn fst $ concat resultsList
        pure $ QueryResults (V.length xs) xs

    mergeQueryResults :: Maybe Int -> [(Component, QueryResults)] -> V.Vector (Component, QueryResult)
    mergeQueryResults mLimit goods =

        let limit = fromMaybe maxBound mLimit

            -- Deduplicate by URI, keeping the highest-scored result, in one pass
            resultMap :: M.Map Text (Component, QueryResult)
            resultMap = foldl' (\m (cmp, QueryResults _ qs) ->
                        V.foldl' (\m' qr -> M.insertWith keepHigher (uri qr) (cmp, qr) m') m qs
                       ) M.empty goods

            -- Sort by score descending and take up to limit
        in V.take limit . V.fromList . sortOn (Down . score . snd) $ M.elems resultMap

        where
        keepHigher :: (Component, QueryResult) -> (Component, QueryResult) -> (Component, QueryResult)
        keepHigher (c1, qr1) (c2, qr2)
            | score qr1 >= score qr2 = (c1, qr1)
            | otherwise              = (c2, qr2)