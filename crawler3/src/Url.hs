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

import Settings

import Data.Text
import Data.Hashable
import Network.URI

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
valText (Url u) = pack . show $ u

derelativise :: Url -> String -> Maybe Url
derelativise baseUrl partialUrl

    | isRelativeReference partialUrl = do
        ref <- parseRelativeReference partialUrl
        let path' = case uriPath ref of
                        ('/':xs) -> '/':xs
                        xs       -> '/':xs
        fromUri $ (val baseUrl) { uriPath     = path'
                                , uriQuery    = uriQuery ref
                                , uriFragment = uriFragment ref
                                }
    | otherwise = mkUrl partialUrl
