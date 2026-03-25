{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , QuasiQuotes #-}

module Query.StructuredProcessor ( StructuredProcessor (..)
                                 , createStructuredProcessor
                                 ) where

import Component                 ( Component )
import Environment               ( Environment (..) )
import Logger                    ( Logger (..) )
import Metadata                  ( Metadata (..), MetadataApi (..) )
import Query.QueryCommon         ( queryComponent, withLocks )
import Query.QueryParser         ( Clause (..), Op (..) )
import Query.QueryProcessorTypes ( UnscoredResults (..), UnscoredResult (..) )
import Registry                  ( Registry (..) )
import Types

import           Control.Concurrent.Async (forConcurrently)
import           Control.Monad            (unless)
import           Data.ByteString          (ByteString)
import           Data.Either              (partitionEithers)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.String.Interpolate  (i)
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

newtype StructuredProcessor =
    StructuredProcessor { runStructured :: CollectionName -> Clause -> IO (Either Text UnscoredResults)
                        }

data Mode = Normal | Regex deriving Eq

createStructuredProcessor :: Environment
                          -> Registry
                          -> MetadataApi
                          -> Logger
                          -> StructuredProcessor
createStructuredProcessor env reg metadataApi lg =
    StructuredProcessor { runStructured = runStructuredImpl env reg metadataApi lg
                        }

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

    runUnscored :: Mode -> [Component] -> Text -> IO (Either [ByteString] UnscoredResults)
    runUnscored _ [] _ = do
        let errMsg = [i|No such collection: #{cn}|]
        infoBs logger [errMsg]
        pure $ Left [errMsg]

    runUnscored mode lockedComponents q = do

        (badss, goods) <- partitionEithers <$> (forConcurrently lockedComponents unscoredQuery)

        let bads = concat badss

        unless (null bads)
               (infoBs logger bads)

        pure $ if null goods
                then Left bads
                else Right $ mconcat goods

        where
        -- Fetching the metadata probably isn't great down here
        -- But we have the handle to the component
        unscoredQuery :: Component -> IO (Either [ByteString] UnscoredResults)
        unscoredQuery lc =

            queryComponent env logger (execParams lc) (encodeUtf8 q) >>= \case

                Left e -> pure . Left $ e
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
