{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , ScopedTypeVariables #-}

module QueryProcessor ( QueryProcessor (..)
                      , createQueryProcessor
                      , runQueryImpl ) where

import Component           ( Component )
import Environment         ( Environment (..) )
import QueryParams         ( QueryParams (..) )
import QueryProcessorTypes ( QueryResults (..), QueryResult (..) )
import Registry            ( Registry (..) )
import Snippets            ( Snippets (lookupSnippet), Snippet (..) )
import Types

import           Control.Concurrent.Async       (forConcurrently)
import           Control.Concurrent.STM         (atomically)
import           Control.Exception.Safe         (catchAnyDeep)
import           Control.Monad                  (unless)
import           Data.Aeson                     (eitherDecodeStrict')
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Either                    (partitionEithers)
import           Data.Heap                      (Heap)
import qualified Data.Heap as H
import           Data.List                      (foldl', sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe                     (fromMaybe)
import           Data.Set                       (toList)
import           Data.Text                      (Text)
import qualified Data.Vector as V
import           GHC.IO.Exception               (ExitCode (..))
import           System.Process.ByteString      (readProcessWithExitCode)
import           Text.Printf                    (printf)

newtype QueryProcessor =
    QueryProcessor { runQuery :: CollectionName -> QueryParams -> IO (Either String QueryResults)
                   }

createQueryProcessor :: Environment
                     -> Registry
                     -> Snippets
                     -> (ByteString -> IO ())
                     -> QueryProcessor
createQueryProcessor env reg snippets lg =
    QueryProcessor { runQuery = runQueryImpl env reg snippets lg
                   }

data ComponentResult = 
    ComponentResult QueryResult Component deriving Show

instance Eq ComponentResult where
    (ComponentResult r1 c1) == (ComponentResult r2 c2) = score r1 == score r2 && c1 == c2

instance Ord ComponentResult where
    compare (ComponentResult r1 c1) (ComponentResult r2 c2) = compare (score r1, c1) (score r2, c2)

runQueryImpl :: Environment
             -> Registry
             -> Snippets
             -> (ByteString -> IO ())
             -> CollectionName
             -> QueryParams
             -> IO (Either String QueryResults)
runQueryImpl env registry snippets logger collectionName@(CollectionName cn) params = do

    -- Take locks
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

            (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> queryComponent lc)

            let errMsg = C8.unlines (map C8.pack bads)
            unless (null bads)
                   (logger errMsg)

            result <- if null goods
                             then pure . Left $ unlines bads
                             else do

                                 let merged = mergeQueryResults (maxResults params) goods

                                 Right <$> attachSnippets merged

            -- Release locks
            mapM_ (releaseLockIO registry) lockedComponents

            pure result

    where
    attachSnippets :: V.Vector (Component, QueryResult) -> IO QueryResults
    attachSnippets vcqrs = do
        xs <- V.forM vcqrs $ \(cmp, qr) -> do
            sn <- fmap sn_snippet <$> lookupSnippet snippets (path cmp) (uri qr)
            pure $ qr { snippet = sn }
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

                         -- Snippets was here!
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
