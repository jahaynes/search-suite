module Gen.Url (genUrl) where

import Data.Maybe
import Network.URI

import           Url
import           Hedgehog
import  Hedgehog.Gen   
import  Hedgehog.Range 

genUrl :: Gen Url
genUrl = fromJust
       . fromUri <$> (URI <$> genScheme
                          <*> (Just <$> genAuthority)
                          <*> genPath
                          <*> genQuery
                          <*> genFragment)

    where
    genScheme = element ["http:", "https:", "ftp:"]

    genAuthority = URIAuth <$> genUser
                           <*> genRegName
                           <*> genPort

    genPath = ('/':) <$> string (constantFrom 1 1 10) alphaNum

    genQuery = pure ""

    genFragment = pure ""

    genUser = pure ""

    genRegName = string (constantFrom 1 1 10) alphaNum

    genPort = show <$> element [80, 8080, 443, 8443] 