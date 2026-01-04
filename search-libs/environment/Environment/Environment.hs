module Environment where

import Data.ByteString (ByteString)

data Env =
    Env { indexerBinary   :: !FilePath
        , collectionsPath :: !FilePath
        , proxySetting    :: !(Maybe (ByteString, Int))
        }

class NewEnvironment m where    -- TODO un-new

    -- getEnv :: m Env

    getCollectionsPath :: m FilePath

    getIndexerBinary   :: m FilePath

    getProxySetting    :: m (Maybe (ByteString, Int))
