module Query.QueryCommon ( withLocks
                         ) where

import Component ( Component )
import Registry  ( Registry (..) )
import Types     ( CollectionName )

import           Control.Concurrent.STM (atomically)
import           Control.DeepSeq        (NFData, deepseq)
import qualified Data.Set as S
import           UnliftIO.Exception     (bracket)

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
