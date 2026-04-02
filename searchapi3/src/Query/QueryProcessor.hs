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

import           Control.Concurrent.Async (forConcurrently)
import           Control.Monad            (unless)
import           Data.ByteString          (ByteString)
import           Data.Either              (partitionEithers)
import           Data.Heap                (Heap)
import qualified Data.Heap as H
import           Data.List                (sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe               (fromMaybe)
import           Data.String.Interpolate  (i)
import           Data.Text                (Text)
import qualified Data.Vector as V
import           Text.Printf              (printf)

newtype QueryProcessor =
    QueryProcessor { runQuery :: CollectionName -> QueryParams -> IO (Either [ByteString] QueryResults)
                   }

createQueryProcessor :: Environment
                     -> Registry
                     -> MetadataApi
                     -> Logger
                     -> QueryProcessor
createQueryProcessor env reg metadataApi lg =
    QueryProcessor { runQuery = runQueryImpl env reg metadataApi lg
                   }

data ComponentResult = 
    ComponentResult QueryResult Component deriving Show

instance Eq ComponentResult where
    (ComponentResult r1 c1) == (ComponentResult r2 c2) = score r1 == score r2 && c1 == c2

instance Ord ComponentResult where
    compare (ComponentResult r1 c1) (ComponentResult r2 c2) = compare (score r1, c1) (score r2, c2)

runQueryImpl :: Environment
             -> Registry
             -> MetadataApi
             -> Logger
             -> CollectionName
             -> QueryParams
             -> IO (Either [ByteString] QueryResults)
runQueryImpl env registry metadataApi logger collectionName@(CollectionName cn) params =

    withLocks registry collectionName $ \lockedComponents ->

        if null lockedComponents

            then
                pure $ Left [[i|No such collection: #{cn}|]]

            else do

                (badss, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> queryComponent env logger (execParams lc) (query params))

                let bads = concat badss

                unless (null bads)
                       (infoBs logger bads)

                if null goods
                    then pure $ Left bads
                    else let merged = mergeQueryResults (maxResults params) goods
                         in Right <$> attachMetadata merged

    where
    execParams :: Component -> [String]
    execParams component =
        concat [ ["query"]
               , case maxResults params of Just n -> [printf "-max_results=%d" n]; Nothing -> []
               , ["--base_path", path component]
               ]

    -- This is interesting
    attachMetadata :: V.Vector (Component, QueryResult) -> IO QueryResults
    attachMetadata vcqrs = do
        xs <- V.forM vcqrs $ \(cmp, qr) -> do
            mmetadata <- lookupMetadata metadataApi (path cmp) (uri qr)
            -- This is in two places
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
                $ M.elems resultMap

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
