{-# LANGUAGE LambdaCase
           , QuasiQuotes #-}

module Bin ( Bin (..)
           , runBs
           , runCbor
           , runJson
           ) where

import           Control.DeepSeq            (NFData)
import           Data.Aeson                 (FromJSON, eitherDecode')
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Functor               ((<&>))
import           Data.String.Interpolate    (i)
import           Data.Text                  (Text)
import           UnliftIO.Exception         (SomeException, catchAnyDeep)
import           Protocol.Encode            (Decode, unlcbor)
import           System.Process.Typed       (ExitCode (ExitSuccess), byteStringInput, closed, proc, readProcess, setStdin)

-- TODO - log every bin run?
data Bin =
    Bin { getCmd   :: !String
        , getArgs  :: ![String]
        , getInput :: !(Maybe ByteString)
        }

runJson :: (FromJSON a, NFData a) => Bin
                                  -> IO (Either [Text] (ByteString, a))
runJson bin =
    runBs bin <&> \case
        Left l -> Left l
        Right (err, out) ->
            case eitherDecode' out of
                Left l  -> Left [ [i|Failed to decode output:|]
                                , [i|#{l}|]
                                , [i|#{out}|] ]
                Right x -> Right (err, x)

runCbor :: Decode a => Bin -> IO (Either [Text] (ByteString, a))
runCbor bin =
    runBs bin <&> \case
        Left l -> Left l
        Right (err, out) -> Right (err, unlcbor out)

runBs :: Bin -> IO (Either [Text] (ByteString, ByteString))
runBs bin =
    catchAnyDeep job (pure . handle)
    where
    job = readProcess ioProcess <&> \(exitCode, out, err) ->
              case exitCode of
                  ExitSuccess -> Right (err, out)
                  _           -> Left [[i|Non-successful error code: #{exitCode}|]]

        where
        ioProcess =
            let process = proc (getCmd bin)
                               (getArgs bin) in
            case getInput bin of
                Nothing -> setStdin closed               process
                Just si -> setStdin (byteStringInput si) process

    handle :: SomeException -> Either [Text] a
    handle ex = Left [ [i|Unexpected exception executing |]
                     , [i|#{getCmd bin} #{getArgs bin}|]
                     , [i|#{ex}|] ]
