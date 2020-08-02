module Storage.MultiMap ( MultiMap(..)
                        , create
                        ) where

import           Control.Concurrent.STM
import           Control.Monad              ((>=>))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict  as M
import           Data.Set                   (Set)
import qualified Data.Set         as S

data MultiMap k v =
    Store { mm_lookupMin :: !(STM (Maybe (k, v)))
          , mm_get       :: !(k -> STM (Maybe v))
          , mm_put       :: !(k -> v -> STM ())
          , mm_delete    :: !(k -> v -> STM ())
          }

create :: (Ord k, Ord v) => IO (MultiMap k v)
create = do
    mm <- newTVarIO M.empty
    pure $ Store { mm_lookupMin = lookupMin mm
                 , mm_get       = get mm
                 , mm_put       = put mm
                 , mm_delete    = delete mm
                 }

lookupMin :: TVar (Map k (Set v)) -> STM (Maybe (k, v))
lookupMin mm = do
    m <- readTVar mm
    case M.lookupMin m of
        Nothing -> pure Nothing
        Just (k, sv) -> pure $
            case S.lookupMin sv of
                Nothing -> Nothing
                Just v  -> Just (k, v)

get :: Ord k => TVar (Map k (Set v)) -> k -> STM (Maybe v)
get mm k = (M.lookup k >=> S.lookupMin) <$> readTVar mm

put :: (Ord k, Ord v) => TVar (Map k (Set v)) -> k -> v -> STM ()
put mm k v = modifyTVar' mm (M.insertWith mappend k (S.singleton v))

delete :: (Ord k, Ord v) => TVar (Map k (Set v)) -> k -> v -> STM ()
delete mm k v = modifyTVar' mm $ M.alter f k
    where
    f   Nothing = Nothing
    f (Just sv) = let sv' = S.delete v sv
                  in if S.null sv'
                      then Nothing
                      else Just sv'