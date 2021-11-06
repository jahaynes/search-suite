module Client.SearchApiClient ( SearchApiClient (..)
                              , create
                              ) where

import           Client.Types         (Doc (..), IndexRequest (..))
import           Errors.Errors        (Error (EncodingError))
import           Network.Poster       (Poster)
import qualified Network.Poster as Po
import           Page.Page            (Page (..))
import           Url                  (Url, valText)

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson                 (encode)
import Data.Text.Encoding         (decodeUtf8')

newtype SearchApiClient m =
    SearchApiClient { postToSearchApi :: Url -> Page -> m () }

create :: IO (SearchApiClient (ExceptT Error IO))
create = do
    poster <- Po.create
    pure $ SearchApiClient { postToSearchApi = postToSearchApiImpl poster }

postToSearchApiImpl :: Monad m => Poster (ExceptT Error m)
                               -> Url
                               -> Page
                               -> ExceptT Error m ()
postToSearchApiImpl poster url page =

    case decodeUtf8' (p_body page) of

        Left _ -> throwE $ EncodingError (p_url page) "Invalid UTF-8"

        Right utf8Content ->

            let req = encode $ IndexRequest [ Doc { d_url     = valText $ p_url page
                                                  , d_content = utf8Content
                                                  }
                                            ]

            in Po.postTo poster url req