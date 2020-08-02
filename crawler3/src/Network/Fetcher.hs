{-# LANGUAGE OverloadedStrings #-}

module Network.Fetcher ( Fetcher (..)
                       , createFetcher
                       ) where

import Errors.Errors
import Page.Page
import Settings
import Url

import Control.Exception.Safe     (SomeException, catchAny, catchAnyDeep)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Crypto.Hash.MD5
import Data.ByteString.Char8      (ByteString, unpack)
import Network.HTTP.Client        
import Network.HTTP.Client.TLS
import Network.HTTP.Types         
import Safe                       (headMay)

newtype Fetcher m =
    Fetcher { fetch :: Url -> m Page }

createFetcher :: IO (Fetcher (ExceptT Error IO))
createFetcher = do

    let settings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro fetchTimeoutMicros }

    Fetcher . fetchImpl <$> newManager settings

fetchImpl :: Manager -> Url -> ExceptT Error IO Page
fetchImpl man url = do

    req <- catchAny (requestFromURI (val url))
                    (throwE . urlError)

    body <- catchAnyDeep (ExceptT $ withResponse req man readResponse)
                         (throwE . fetchError)

    let md5 = hash body

    liftIO $ print url

    pure $ Page { p_url  = url 
                , p_body = body
                , p_md5  = md5
                }

    where
    readResponse :: Response BodyReader -> IO (Either Error ByteString)
    readResponse resp =
        case do checkStatus resp
                checkSize   resp
                checkType   resp of  -- dispatch on type here
                    Left e   -> pure $ Left e
                    Right () -> Right . mconcat <$> (brConsume . responseBody $ resp)

    urlError :: SomeException -> Error
    urlError ex = FetchError { fe_url           = url
                             , fe_simpleMessage = "Invalid url"
                             , fe_statusCode    = Nothing
                             , fe_exception     = Just ex
                             }

    fetchError :: SomeException -> Error
    fetchError ex = FetchError { fe_url           = url
                               , fe_simpleMessage = "Could not reach url"
                               , fe_statusCode    = Nothing
                               , fe_exception     = Just ex
                               }

    checkStatus :: Response a -> Either Error ()
    checkStatus resp =
        let code = statusCode (responseStatus resp)
        in if code >= 200 && code < 300
               then Right ()
               else Left $
                   FetchError { fe_url           = url
                              , fe_simpleMessage = "Bad status code"
                              , fe_statusCode    = Just code
                              , fe_exception     = Nothing
                              }

    checkSize :: Response a -> Either Error ()
    checkSize _ = Right () -- TODO

    checkType :: Response a -> Either Error () -- Return the type here
    checkType resp =
        case headMay
           . filter (\h -> fst h == hContentType)
           . responseHeaders
           $ resp of

               Just (_, "text/html; charset=UTF-8")     -> Right ()
               Just (_, "text/html;charset=UTF-8")      -> Right ()
               Just (_, "text/html; charset=utf-8")     -> Right ()
               Just (_, "text/html; charset=\"utf-8\"") -> Right ()
               Just (_, "text/html")                    -> Right ()
               Just (_, "text/html;charset=utf-8")      -> Right ()

               Nothing ->
                   Left $ FetchError { fe_url           = url
                                     , fe_simpleMessage = "No Content-Type header"
                                     , fe_statusCode    = Nothing
                                     , fe_exception     = Nothing
                                     }              

               Just (_, uh) ->
                   Left $ FetchError { fe_url           = url
                                     , fe_simpleMessage = "Unknown Content-Type header: " <> unpack uh
                                     , fe_statusCode    = Nothing
                                     , fe_exception     = Nothing
                                     }  
