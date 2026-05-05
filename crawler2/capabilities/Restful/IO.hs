{-# LANGUAGE QuasiQuotes #-}

module Restful.IO ( fetchGetImpl ) where

import Restful.Types

import Control.Exception.Safe    (MonadCatch, SomeException, catchAnyDeep)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.ByteString.Lazy      (toStrict)
import Data.String.Interpolate   (i)
import Data.Text                 (Text)
import Network.HTTP.Client       (Manager, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)

fetchGetImpl :: (MonadCatch m, MonadIO m) => Manager -> Url -> m (Either [Text] Response)
fetchGetImpl http url =

    case parseRequest (show url) of

        Left ex ->

            left [ [i|Could not parse url: #{url}|]
                 , [i|#{ex}|] ]

        Right req ->

            let job = do
                    liftIO $ putStrLn [i|Fetching: #{url}|] -- TODO log.  Or lift outside
                    resp <- httpLbs req http

                    right Response { getUrl  = url
                                   , getCode = statusCode $ responseStatus resp
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
