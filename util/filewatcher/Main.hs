{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Control.Concurrent       (threadDelay)
import Control.Monad            (forever)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8 -- TODO unicode
import GHC.Generics             (Generic)
import Network.HTTP.Client as H
import System.FSNotify     as F (Event (..), EventIsDirectory (..), watchTree, withManager)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Types

data Config =
    Config { dir  :: !FilePath
           , ext  :: !String
           , coln :: !String
           }

newtype IndexReq =
    IndexReq { docs :: [Doc] }
        deriving Generic

data Doc = Doc
    { url     :: !String
    , content :: !T.Text
    } deriving Generic  -- TODO bytestrings

myConfig :: Config
myConfig = Config { dir  = "."
                  , ext  = ".cs"
                  , coln = "watched" -- TODO actually use
                  }

main :: IO ()
main = do

    http    <- H.newManager defaultManagerSettings
    baseReq <- parseRequest "http://127.0.0.1:8081/indexPage/watched"

    F.withManager $ \mgr -> do
        _ <- watchTree
            mgr
            (dir myConfig)
            (fileEventEndsWith (ext myConfig))
            (fileChanged http baseReq)
        forever $ threadDelay 1000000

fileChanged :: Manager -> Request -> Event -> IO ()
fileChanged http baseReq e = do

    let path = eventPath e

    body <- TIO.readFile path

    let req = baseReq { method = "POST"
                      , requestBody = RequestBodyLBS $ encode body }

    response <- httpLbs req http
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response

fileEventEndsWith :: String -> Event -> Bool
fileEventEndsWith end e = eventIsDirectory e == IsFile
                       && endsWith (eventPath e)

    where
    endsWith :: String -> Bool
    endsWith name = reverse end == take (length end) (reverse name)