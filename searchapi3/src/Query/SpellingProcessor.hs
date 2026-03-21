{-# LANGUAGE OverloadedStrings
           , QuasiQuotes #-}

module Query.SpellingProcessor ( SpellingProcessor (..)
                               , createSpellingProcessor
                               ) where

import Component                 ( Component )
import Environment               ( Environment (..) )
import Logger                    ( Logger (..) )
import Query.QueryCommon         ( withLocks )
import Query.QueryProcessorTypes ( SpellingSuggestions (..) )
import Registry                  ( Registry (..) )
import Types

import           Control.Concurrent.Async       (forConcurrently)
import           Control.Exception.Safe         (catchAnyDeep)
import           Control.Monad                  (unless)
import           Data.Aeson                     (eitherDecodeStrict')
import qualified Data.ByteString.Char8 as C8
import           Data.Either                    (partitionEithers)
import           Data.Functor                   ((<&>))
import           Data.Maybe                     (fromMaybe)
import           Data.String.Interpolate        (i)
import           Data.Text                      (Text)
import qualified Data.Text as T
import           GHC.IO.Exception               (ExitCode (..))
import           System.Process.ByteString      (readProcessWithExitCode)
import           UnliftIO.Exception             (SomeException)

newtype SpellingProcessor =
    SpellingProcessor { runSpelling :: CollectionName -> Text -> Maybe Int -> IO (Either Text SpellingSuggestions) }

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
