{-# LANGUAGE InstanceSigs #-}

module Url ( Url
           , mkUrl
           , derelativise
           , getHost
           , val
           , valText
           , fromUri
           , Host (Host)
           ) where

import           Data.Text      (Text)
import qualified Data.Text as T
import           Data.Hashable
import           Network.URI

newtype Url =
    Url URI deriving (Eq, Ord)

instance Show Url where
    show (Url u) = show u

instance Hashable Url where
    hashWithSalt :: Int -> Url -> Int
    hashWithSalt x u = hashWithSalt x (show u)

newtype Host =
    Host String deriving (Eq, Ord)

instance Hashable Host where
    hashWithSalt :: Int -> Host -> Int
    hashWithSalt x (Host h) = hashWithSalt x h

mkUrl :: String -> Maybe Url
mkUrl rawUrl = fromUri =<< parseURIReference rawUrl

stripFragments :: Bool -- TODO settings
stripFragments = True

fromUri :: URI -> Maybe Url
fromUri uri = do
    _   <- uriAuthority uri
    pure $ if stripFragments
        then Url $! uri { uriFragment = "" }
        else Url uri

getHost :: Url -> Host
getHost (Url uri) =
    let Just auth = uriAuthority uri
    in Host (uriRegName auth)

val :: Url -> URI
val (Url u) = u

valText :: Url -> Text
valText (Url u) = T.pack . show $ u

derelativise :: Maybe Url -> Url -> String -> Maybe Url
derelativise mBaseHref baseUrl@(Url bu) relativeOrAbsoluteUrl

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

        fromUri $ (val baseUrl) { uriPath     = path''
                                , uriQuery    = uriQuery ref
                                , uriFragment = uriFragment ref
                                }

    | otherwise = mkUrl relativeOrAbsoluteUrl
