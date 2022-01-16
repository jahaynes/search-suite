{-# LANGUAGE DataKinds,
             TypeOperators #-}

module Controllers.Diagnostic where

import Registry (Registry (totalLocksHeld, totalNumComponents))

import Servant

type DiagnosticApi = "diagnostic" :> "totalNumComponents" :> Get '[JSON] Int
                :<|> "diagnostic" :> "totalLocksHeld"     :> Get '[JSON] Int

diagnosticApi :: Proxy DiagnosticApi
diagnosticApi = Proxy

diagnosticServer :: Registry
                 -> ServerT DiagnosticApi IO 
diagnosticServer registry
    = totalNumComponents registry
 :<|> totalLocksHeld registry

