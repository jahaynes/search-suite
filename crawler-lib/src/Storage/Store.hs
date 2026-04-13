module Storage.Store ( Store (..)
                     , create
                     ) where

import           Control.Concurrent.STM     (atomically)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Hashable              (Hashable)
import qualified ListT                      (toList)
import           StmContainers.Set          (Set)
import qualified StmContainers.Set as S

data Store a m = 
    Store { s_member :: !(a -> m Bool)
          , s_put    :: !(a -> m ())
          , s_toList :: !(m [a])
          }

create :: (Eq a, Hashable a, MonadIO m) => IO (Store a m)
create = do
    s <- liftIO S.newIO
    pure $ Store { s_member = member s
                 , s_put    = set s
                 , s_toList = toList s
                 }

member :: (Eq a, Hashable a, MonadIO m) => Set a -> a -> m Bool
member s x = liftIO . atomically $ S.lookup x s

set :: (Eq a, Hashable a, MonadIO m) => Set a -> a -> m ()
set s x = liftIO . atomically $ S.insert x s

toList :: MonadIO m => Set a -> m [a]
toList = liftIO . atomically . ListT.toList . S.listT
