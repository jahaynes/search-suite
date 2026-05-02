
{-# LANGUAGE DeriveGeneric
           , GeneralizedNewtypeDeriving
           , InstanceSigs #-}

module Restful.Types ( Host (..)
                     , Response (..)
                     , Url
                     , derelativise
                     , getHost
                     , mkUrl
                     ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Hashable   (Hashable (..))
import Data.Text       (Text, pack, unpack)
import Data.Functor
import GHC.Generics    (Generic)
import Network.URI

data Response =
    Response { getUrl  :: !Url
             , getCode :: !Int
             , getBody :: !ByteString
             } deriving (Generic, Show)

instance NFData Response

newtype Url =
    Url URI deriving (Generic, Eq, Ord)

instance NFData Url

instance Hashable Url where
    hashWithSalt s u = hashWithSalt s (show u)

instance Show Url where
    show (Url u) = show u

newtype Host =
    Host Text deriving (Eq, Ord, Hashable)

instance Show Host where
    show (Host t) = unpack t

mkUrl :: String -> Maybe Url
mkUrl rawUrl = fromUri =<< parseURIReference rawUrl

fromUri :: URI -> Maybe Url
fromUri uri = uriAuthority uri $> (Url . rectify $ uri)

getHost :: Url -> Maybe Host
getHost (Url uri) = Host . pack . uriRegName <$> uriAuthority uri

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
