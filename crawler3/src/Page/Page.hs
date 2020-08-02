module Page.Page where

import Url

import Data.ByteString (ByteString)

data Page =
    Page { p_url  :: !Url
         , p_body :: !ByteString
         , p_md5  :: !ByteString
         }