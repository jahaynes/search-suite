{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , QuasiQuotes
           , ScopedTypeVariables #-}

module Query.QueryProcessor ( QueryProcessor (..)
                            , createQueryProcessor
                            ) where

import Component                 ( Component )
import Environment               ( Environment (..) )
import Logger                    ( Logger (..) )
import Metadata                  ( Metadata (..), MetadataApi (..) )
import Query.QueryParams         ( QueryParams (..) )
import Query.QueryParser         ( Clause (..), Op (..) )
import Query.QueryProcessorTypes ( SpellingSuggestions (..), QueryResults (..), QueryResult (..), UnscoredResults (..), UnscoredResult (..) )
import Registry                  ( Registry (..) )
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
import           Data.Functor                   ((<&>))
import           Data.Heap                      (Heap)
import qualified Data.Heap as H
import           Data.List                      (sortOn)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map.Strict as M
import           Data.Maybe                     (fromMaybe)
import qualified Data.Set as S
import           Data.String.Interpolate        (i)
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import qualified Data.Vector as V
import           GHC.IO.Exception               (ExitCode (..))
import           System.Process.ByteString      (readProcessWithExitCode)
import           Text.Printf                    (printf)
import           UnliftIO.Exception             (SomeException, bracket)

data QueryProcessor =
    QueryProcessor { runQuery      :: !(CollectionName -> QueryParams -> IO (Either String QueryResults))
                   , runSpelling   :: !(CollectionName -> Text -> Maybe Int -> IO (Either Text SpellingSuggestions))
                   , runStructured :: !(CollectionName -> Clause -> IO (Either Text UnscoredResults))
                   }

createQueryProcessor :: Environment
                     -> Registry
                     -> MetadataApi
                     -> Logger
                     -> QueryProcessor
createQueryProcessor env reg metadataApi lg =
    QueryProcessor { runQuery      = runQueryImpl env reg metadataApi lg
                   , runSpelling   = runSpellingImpl env reg lg
                   , runStructured = runStructuredImpl env reg metadataApi lg
                   }

data ComponentResult = 
    ComponentResult QueryResult Component deriving Show

instance Eq ComponentResult where
    (ComponentResult r1 c1) == (ComponentResult r2 c2) = score r1 == score r2 && c1 == c2

instance Ord ComponentResult where
    compare (ComponentResult r1 c1) (ComponentResult r2 c2) = compare (score r1, c1) (score r2, c2)

data Mode = Normal | Regex deriving Eq

-- TODO, this can probably pushed into indexer-qp2
runStructuredImpl :: Environment
                  -> Registry
                  -> MetadataApi
                  -> Logger
                  -> CollectionName
                  -> Clause
                  -> IO (Either Text UnscoredResults)
runStructuredImpl env reg metadataApi logger collectionName@(CollectionName cn) clause =
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

    runUnscored :: Mode -> [Component] -> Text -> IO (Either ByteString UnscoredResults)
    runUnscored _ [] _ = do
        let errMsg = [i|No such collection: #{cn}|]
        infoBs logger [errMsg]
        pure $ Left errMsg
    
    runUnscored mode lockedComponents q = do
        (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents unscoredQuery)
        let errMsg = C8.unlines (map C8.pack bads)
        unless (null bads)
               (infoBs logger [errMsg])
        pure $ if null goods
                then Left errMsg
                else Right $ mconcat goods

        where
        -- Fetching the metadata probably isn't great down here
        -- But we have the handle to the component
        unscoredQuery lc =

            queryComponent env logger (execParams lc) (encodeUtf8 q) >>= \case

                Left e -> error e
                Right (UnscoredResults n qrs) -> do
                    -- TODO probably remove the set?
                    qrs' <- mapM attachMetadata $ S.toList qrs
                    pure $ Right (UnscoredResults n (S.fromList $ qrs'))

            where
            attachMetadata :: UnscoredResult -> IO UnscoredResult
            attachMetadata ur =
                lookupMetadata metadataApi (path lc) (ur_uri ur) >>= \case
                    Nothing -> undefined -- TODO
                    Just m -> pure ur { ur_metadata = Just . M.delete "uri" $ unMetadata m }

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

runSpellingImpl :: Environment
                -> Registry
                -> Logger
                -> CollectionName
                -> Text
                -> Maybe Int
                -> IO (Either Text SpellingSuggestions)
runSpellingImpl env registry logger collectionName@(CollectionName cn) s mMaxDist =

    withLocks registry collectionName $ \lockedComponents -> do

        if null lockedComponents

            then pure $ Left [i|No such collection: #{cn}|]

            else do

                (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> spellingComponent lc)

                let errMsg = T.unlines bads
                unless (null bads)
                       (infoBs logger . (\l -> [l]) . C8.pack . T.unpack $ errMsg) -- TODO: #logging

                pure $ if null goods
                           then Left errMsg
                           else Right . mconcat . map snd $ goods

    where
    spellingComponent :: Component -> IO (Either Text SpellingSuggestions)
    spellingComponent cmp =

        let maxDist = fromMaybe 1 mMaxDist

            -- TODO better normalisation
            args = ["spelling", path cmp, show maxDist] ++ (map T.unpack $ T.words s)

            job = readProcessWithExitCode (indexerBinary env) args "" <&> \(exitcode, stdout, stderr) ->
                      case exitcode of
                          ExitSuccess ->
                              case eitherDecodeStrict' stdout of
                                  Left e ->
                                      Left [i|Could not parse result of spelling: #{e}|]
                                  Right ss ->
                                      Right $ SpellingSuggestions ss
                          _ -> Left [i|Spelling did not execute successfully: #{exitcode}\n#{stderr}|]

        in catchAnyDeep job (pure . handle)

        where
        handle :: SomeException -> Either Text a
        handle e = Left [i|Exception when executing spelling: #{e}|]


runQueryImpl :: Environment
             -> Registry
             -> MetadataApi
             -> Logger
             -> CollectionName
             -> QueryParams
             -> IO (Either String QueryResults)
runQueryImpl env registry metadataApi logger collectionName@(CollectionName cn) params =

    withLocks registry collectionName $ \lockedComponents -> do

        if null lockedComponents

            then do
                let errMsg = printf "No such collection: %s" cn
                pure $ Left errMsg

            else do

                (bads, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> queryComponent env logger (execParams lc) (query params))

                let errMsg = C8.unlines (map C8.pack bads)
                unless (null bads)
                       (infoBs logger [errMsg])

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

queryComponent :: (FromJSON a, NFData a) => Environment
                                         -> Logger
                                         -> [String]
                                         -> ByteString
                                         -> IO (Either String a)
queryComponent env logger execParams queryStr =

    let job = do logExecution

                 (exitcode, stdout, stderr) <- readProcessWithExitCode (indexerBinary env) execParams queryStr

                 case exitcode of

                     ExitSuccess -> do
                         C8.putStr stderr -- TODO don't
                         Right <$> decodeOutput stdout

                     _  -> do
                        infoBs logger ["stdout was: " <> stdout]
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
    logExecution = infoBs logger
                 . (\l -> [l])
                 . C8.pack
                 $ unwords [ "Running binary: "
                           , show (indexerBinary env)
                           , show execParams
                           ]
