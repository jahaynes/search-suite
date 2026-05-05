module Main ( main ) where

main :: IO ()
main = pure ()

{-
    ps <- V.replicateM 4 P.new

    case mkUrl "http://127.0.0.1:3000" of
        
        Nothing -> error "Bad url"
        
        Just url -> do

            addUrl ps url

            js <- forM ps $ \p -> async $ do

                http <- newManager defaultManagerSettings
                
                go http p ps

            mapM_ wait js
-}