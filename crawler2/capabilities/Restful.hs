{-# LANGUAGE DeriveGeneric,
             QuasiQuotes #-}

module Restful ( Manager
               , Response (..)
               , Restful (..)
               , fetchGetImpl
               ) where

import Control.DeepSeq           (NFData)
import Control.Exception.Safe
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.ByteString           (ByteString)
import Data.ByteString.Lazy      (toStrict)
import Data.String.Interpolate   (i)
import Data.Text                 (Text)
import GHC.Generics              (Generic)
import Network.HTTP.Client       (Manager, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)

class Restful m where

    fetchGet :: String -> m (Either [Text] Response)

data Response =
    Response { getCode :: !Int
             , getBody :: !ByteString
             } deriving Generic

instance NFData Response

fetchGetImpl :: (MonadCatch m, MonadIO m) => Manager -> String -> m (Either [Text] Response)
fetchGetImpl http url =

    case parseRequest url of

        Left ex ->

            left [ [i|Could not parse url: #{url}|]
                 , [i|#{ex}|] ]

        Right req ->

            let job = do
                    resp <- httpLbs req http
                    right Response { getCode = statusCode $ responseStatus resp
                                   , getBody = toStrict $ responseBody resp
                                   }
            in catchAnyDeep (liftIO job) (left . asErrMsg)

    where
    asErrMsg :: SomeException -> [Text]
    asErrMsg ex = [ [i|HTTP failure fetching #{url}|]
                  , [i|#{ex}|] ]

left :: Applicative m => [Text] -> m (Either [Text] r)
left = pure . Left

right :: Applicative m => r -> m (Either [Text] r)
right = pure . Right