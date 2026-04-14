
{-# LANGUAGE DeriveGeneric
           , InstanceSigs #-}

module Restful.Types ( Response (..)
                     , Url (..)
                     , derelativise
                     , mkUrl
                     ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import GHC.Generics    (Generic)
import Network.URI

data Response =
    Response { getUrl  :: !Url
             , getCode :: !Int
             , getBody :: !ByteString
             } deriving (Generic, Show)

instance NFData Response

newtype Url =
    Url URI deriving Generic

instance NFData Url

instance Show Url where
    show (Url u) = show u

mkUrl :: String -> Maybe Url
mkUrl rawUrl = fromUri =<< parseURIReference rawUrl

fromUri :: URI -> Maybe Url
fromUri uri = do
    _ <- uriAuthority uri
    pure $ Url uri

derelativise :: Maybe Url -> Url -> String -> Maybe Url
derelativise mBaseHref (Url bu) relativeOrAbsoluteUrl

    | isRelativeReference relativeOrAbsoluteUrl = do

        ref <- parseRelativeReference relativeOrAbsoluteUrl

        -- Ensure relative path starts with a '/'
        let (whole, path') = case uriPath ref of
                                 ('/':xs) -> (True,  '/':xs)
                                 xs       -> (False, '/':xs)

        -- Ensure base path does not end with a '/'
        let base = case mBaseHref of
                       Nothing             -> reverse . dropWhile (=='/') . reverse $ uriPath bu
                       Just (Url baseHref) -> reverse . dropWhile (=='/') . reverse $ uriPath baseHref

        -- Take the combined path or start again from root
        let path'' = if whole
                         then path'
                         else base <> path'

        fromUri bu { uriPath     = path''
                   , uriQuery    = uriQuery ref
                   , uriFragment = uriFragment ref
                   }

    | otherwise = mkUrl relativeOrAbsoluteUrl
