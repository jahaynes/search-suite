{-# LANGUAGE OverloadedStrings #-}

module Network.Poster ( Poster (..)
                      , create
                      ) where

import Errors.Errors
import Settings
import Url           (Url, val)

import Control.Exception.Safe     (SomeException, handleAnyDeep)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString.Lazy       (ByteString)
import Network.HTTP.Client
import Network.HTTP.Types         (statusCode)

newtype Poster m =
    Poster { postTo :: Url -> ByteString -> m () 
           }

create :: IO (Poster (ExceptT Error IO))
create = do
    man <- liftIO $ newManager defaultManagerSettings
    pure $ Poster { postTo = postToImpl man }

postToImpl :: Manager -> Url -> ByteString -> ExceptT Error IO ()
postToImpl man dest content = do

    code <- handleAnyDeep (throwE . postError) $ do

                req <- (\r -> r { method          = "POST"
                                , requestBody     = RequestBodyLBS content
                                , responseTimeout = responseTimeoutMicro posterTimeoutMicros
                                } ) <$> requestFromURI (val dest)

                resp <- liftIO (httpNoBody req man)

                pure . statusCode . responseStatus $ resp

    checkStatus code

    where
    postError :: SomeException -> Error
    postError ex = PostError { pe_simpleMessage = "Could not post"
                             , pe_statusCode    = Nothing
                             , pe_exception     = Just ex
                             }

    checkStatus :: Int -> ExceptT Error IO ()
    checkStatus code
        | code >= 200 && code < 300 = pure ()
        | otherwise = throwE $ PostError { pe_simpleMessage = "Bad status code on post"
                                         , pe_statusCode    = Just code
                                         , pe_exception     = Nothing
                                         }