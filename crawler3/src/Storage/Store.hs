module Storage.Store ( Store (..)
                     , create
                     ) where

import           Control.Concurrent.STM     (atomically)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Hashable              (Hashable)
import qualified ListT                      (toList)
import           StmContainers.Set          (Set)
import qualified StmContainers.Set as S

data Store a m = 
    Store { s_member :: !(a -> m Bool)
          , s_put    :: !(a -> m ())
          , s_toList :: !(m [a])
          }

create :: (Eq a, Hashable a, MonadIO m) => m (Store a (ExceptT e IO))
create = do
    s <- liftIO S.newIO
    pure $ Store { s_member = member s
                 , s_put    = set s
                 , s_toList = toList s
                 }

member :: (Eq a, Hashable a) => Set a -> a -> ExceptT e IO Bool
member s x = liftIO . atomically $ S.lookup x s

set :: (Eq a, Hashable a) => Set a -> a -> ExceptT e IO ()
set s x = liftIO . atomically $ S.insert x s

toList :: Set a -> ExceptT e IO [a]
toList = liftIO . atomically . ListT.toList . S.listT
