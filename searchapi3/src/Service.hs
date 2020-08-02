module Service ( Service (..) 
               , createService
               ) where

import           Api                       ( Doc )
import           Compactor                 ( Compactor, createCompactor )
import qualified Compactor           as Cp ( Compactor (..) )
import           Environment               ( Environment )
import           Importer                  ( Importer, createImporter )
import qualified Importer            as Im ( Importer (importCollection) )
import           Indexer                   ( Indexer, createIndexer )
import qualified Indexer             as Ix ( Indexer (..) )
import           QueryParams               ( QueryParams (..) )
import           QueryProcessor            ( QueryProcessor, createQueryProcessor )
import qualified QueryProcessor      as Qp ( QueryProcessor (runQuery) )
import           QueryProcessorTypes
import           Registry                  ( Registry, createRegistry )
import qualified Registry            as Rg ( Registry (..) )
import           Types                     ( CollectionName, Logger (..) )
import           WarcFileReader            ( WarcFileReader, createWarcFileReader )
import qualified WarcFileWriter      as Ww ( createWarcFileWriter )

import Data.ByteString (ByteString)

data System =
    System { registry       :: !Registry
           , compactor      :: !Compactor
           , importer       :: !Importer
           , indexer        :: !Indexer
           , queryProcessor :: !QueryProcessor
           , warcFileReader :: !WarcFileReader
           }

data Service =
    Service { importCollection   :: !(CollectionName -> IO (Either ByteString ()))
            , indexDocuments     :: !(CollectionName -> [Doc] -> IO (Either String Int))
            , indexLocalWarcFile :: !(CollectionName -> FilePath -> IO (Either ByteString ()))
            , runQuery           :: !(CollectionName -> QueryParams -> IO (Either ByteString QueryResults))
            , mergeInto          :: !(CollectionName -> CollectionName -> IO ())
            -- Diagnostics
            , totalNumComponents :: !(IO Int)
            , totalLocksHeld     :: !(IO Int)
            }

createService :: Environment 
              -> (Logger -> ByteString -> IO ())
              -> IO Service
createService env logger = do

    reg <- createRegistry env (logger RegistryLogger)

    let wfr = createWarcFileReader 128 (logger WarcFileReaderLogger)

    let wfw = Ww.createWarcFileWriter

    let cpc = createCompactor env reg wfw (logger CompactorLogger)

    let system = System { registry       = reg 
                        , compactor      = cpc
                        , importer       = createImporter env reg cpc
                        , indexer        = createIndexer env wfr wfw cpc reg
                        , queryProcessor = createQueryProcessor env reg wfr (logger QueryProcessorLogger)
                        , warcFileReader = wfr
                        }

    pure $ Service { importCollection   = Im.importCollection (importer system)
                   , indexDocuments     = Ix.indexDocuments (indexer system)
                   , indexLocalWarcFile = Ix.indexLocalWarcFile (indexer system)
                   , runQuery           = Qp.runQuery (queryProcessor system)
                   , mergeInto          = Cp.mergeInto cpc
                   , totalNumComponents = Rg.totalNumComponents reg
                   , totalLocksHeld     = Rg.totalLocksHeld reg
                   }
