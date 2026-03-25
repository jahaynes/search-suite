{-# LANGUAGE LambdaCase
           , OverloadedStrings #-}

module Query.QueryCommon ( queryComponent
                         , withLocks
                         ) where

import Bin         ( Bin (..), runJson )
import Component   ( Component )
import Environment ( Environment (..) )
import Logger      ( Logger (..) )
import Registry    ( Registry (..) )
import Types       ( CollectionName )

import           Control.Concurrent.STM      (atomically)
import           Control.DeepSeq             (NFData, deepseq)
import           Data.Aeson                  (FromJSON)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor                ((<&>))
import qualified Data.Set as S
import           UnliftIO.Exception          (bracket)

withLocks :: NFData a => Registry -> CollectionName -> ([Component] -> IO a) -> IO a
withLocks reg collectionName f =
    bracket acquire release $ \cmps -> do
        y <- f cmps -- TODO: should this be catchAnyDeep
        y `deepseq` pure y
    where
    acquire = atomically $ do
        components <- S.toList <$> viewCollectionComponents reg collectionName
        mapM_ (takeLock reg) components
        pure components
    release = mapM_ (releaseLockIO reg)

queryComponent :: (FromJSON a, NFData a) => Environment
                                         -> Logger
                                         -> [String]
                                         -> ByteString
                                         -> IO (Either [ByteString] a) -- Does it need to be so abstract?
queryComponent env logger execParams queryStr =

    let bin = Bin { getCmd   = indexerBinary env
                  , getArgs  = execParams
                  , getInput = Just $ LBS.fromStrict queryStr } -- conversion needed?

    in runJson bin <&> \case
        Left l           -> Left l
        Right (err, out) -> Right out
