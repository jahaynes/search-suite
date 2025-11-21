{-# LANGUAGE MultiParamTypeClasses,
             OverloadedStrings #-}

-- TODO: not used?
module Controllers.Mime where

import Data.ByteString.Lazy              (fromStrict)
import Data.Text                         (Text)
import Data.Text.Encoding                (encodeUtf8)
import Network.HTTP.Media                ((//), (/:))
import Servant                           (Accept (..), MimeRender (..))

data Html

instance Accept Html where
   contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender Html Text where
   mimeRender _ val = fromStrict $ encodeUtf8 val
