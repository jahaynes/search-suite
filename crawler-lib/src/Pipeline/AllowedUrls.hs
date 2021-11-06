{-# LANGUAGE MultiWayIf #-}

module Pipeline.AllowedUrls where

import Storage.Store as S (Store (..), create)
import Url                (Host (..), Url, getHost)

import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.List
import Data.List.Split
import Text.Regex.TDFA

data AllowedUrls m =
    AllowedUrls { allowUrlAndVariants :: !(Url -> m ())
                , urlAllowed          :: !(Url -> m Bool)
                }

create :: MonadIO m => m (AllowedUrls (ExceptT e IO))
create = do

    store <- S.create

    pure AllowedUrls { allowUrlAndVariants = allowUrlAndVariantsImpl store
                     , urlAllowed          = urlAllowedImpl store
                     }

allowUrlAndVariantsImpl :: Store String (ExceptT e IO)
                        -> Url
                        -> ExceptT e IO ()
allowUrlAndVariantsImpl store url = do
    let variants = buildVariantRegex url
    liftIO . putStrLn $ variants
    S.s_put store variants

buildVariantRegex :: Url -> String
buildVariantRegex url = do

    let url' = replace "." "\\." . show $ url

    let host' = let (Host host) = getHost url in replace "." "\\." host

    if | "http://www\\." `isPrefixOf` url' ->
             mconcat [ "^http:\\/\\/(:?[^.]+\\.)"
                     , drop (length "www\\.") host'
                     , "(:?\\/.*)?"
                     ]

       | "https://www\\." `isPrefixOf` url' ->
             mconcat [ "^https:\\/\\/(:?[^.]+\\.)"
                     , drop (length "www\\.") host'
                     , "(:?\\/.*)?"
                     ]

       | "http://" `isPrefixOf` url' ->
             mconcat [ "^http:\\/\\/(:?[^.]+\\.)"
                     , host'
                     , "(:?\\/.*)?"
                     ]

       | "https://" `isPrefixOf` url' ->
             mconcat [ "^https:\\/\\/(:?[^.]+\\.)"
                     , host'
                     , "(:?\\/.*)?"
                     ]

       | otherwise -> error "unrecognised"

    where
    replace :: String -> String -> String -> String
    replace src dest = intercalate dest . splitOn src

urlAllowedImpl :: Store String (ExceptT e IO)
               -> Url
               -> ExceptT e IO Bool
urlAllowedImpl store url = do

    let url' = show url

    patterns <- s_toList store

    pure $ any (\p -> url' =~ p) patterns
