{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Controllers.WarcIndexation where

import Extensions.WarcIndexer (WarcIndexer (..))
import Types                  (CollectionName)

import Servant

type WarcIndexationApi = "indexLocalWarcFile" :> Capture "col" CollectionName
                                              :> ReqBody '[PlainText] String
                                              :> Post '[JSON] (Either String ())

warcIndexationApi :: Proxy WarcIndexationApi
warcIndexationApi = Proxy

warcIndexationServer :: WarcIndexer
                     -> ServerT WarcIndexationApi IO 
warcIndexationServer warcIndexer
    = indexLocalWarcFile warcIndexer

