{-# LANGUAGE LambdaCase
           , QuasiQuotes #-}

module Query.SpellingProcessor ( SpellingProcessor (..)
                               , createSpellingProcessor
                               ) where

import Bin                       ( Bin (..), runJson )
import Component                 ( Component )
import Environment               ( Environment (..) )
import Logger                    ( Logger (..) )
import Query.QueryCommon         ( withLocks )
import Query.QueryProcessorTypes ( SpellingSuggestions (..) )
import Registry                  ( Registry (..) )
import Types

import           Control.Concurrent.Async (forConcurrently)
import           Control.Monad            (unless)
import           Data.ByteString          (ByteString)
import           Data.Either              (partitionEithers)
import           Data.Functor             ((<&>))
import           Data.Maybe               (fromMaybe)
import           Data.String.Interpolate  (i)
import           Data.Text                (Text)
import qualified Data.Text as T

newtype SpellingProcessor =
    SpellingProcessor { runSpelling :: CollectionName -> Text -> Maybe Int -> IO (Either [ByteString] SpellingSuggestions) }

createSpellingProcessor :: Environment
                        -> Registry
                        -> Logger
                        -> SpellingProcessor
createSpellingProcessor env reg lg =
    SpellingProcessor { runSpelling = runSpellingImpl env reg lg
                      }

runSpellingImpl :: Environment
                -> Registry
                -> Logger
                -> CollectionName
                -> Text
                -> Maybe Int
                -> IO (Either [ByteString] SpellingSuggestions)
runSpellingImpl env registry logger collectionName@(CollectionName cn) s mMaxDist =

    withLocks registry collectionName $ \lockedComponents ->

        if null lockedComponents

            then pure $ Left [[i|No such collection: #{cn}|]]

            else do

                (badss, goods) <- partitionEithers <$> (forConcurrently lockedComponents $ \lc -> fmap (\qr -> (lc, qr)) <$> spellingComponent lc)

                let bads = concat badss

                unless (null bads)
                       (infoBs logger bads)

                pure $ if null goods
                           then Left bads
                           else Right . mconcat . map snd $ goods

    where
    spellingComponent :: Component -> IO (Either [ByteString] SpellingSuggestions)
    spellingComponent cmp =

        let maxDist = fromMaybe 1 mMaxDist

            -- TODO better normalisation
            execParams = ["spelling", path cmp, show maxDist] ++ (map T.unpack $ T.words s)

            bin = Bin { getCmd   = indexerBinary env
                      , getArgs  = execParams
                      , getInput = Nothing }

        in runJson bin <&> \case
               Left l           -> Left l
               Right (err, out) -> Right $ SpellingSuggestions out
