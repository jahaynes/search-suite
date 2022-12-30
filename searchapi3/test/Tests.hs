{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Api                    (Doc (..))
import Compactor              (Compactor, createCompactor)
import CompactorStrategy      (fibSet, hybridStrategy)
import Component              (Component (..))
import Environment            (Environment (..))
import Importer               (Importer, createImporter)
import Indexer                (Indexer (..), createIndexer)
import QueryParams            (QueryParams (..))
import QueryProcessor         (QueryProcessor (..), createQueryProcessor)
import QueryProcessorTypes    (QueryResults (..)) 
import Registry               (Registry (..), createRegistry)
import Snippets               (Snippets, createSnippets)
import System.Directory       (removeDirectoryRecursive)
import Types                  (CollectionName (..), numDocs)
import WarcFileReader         (WarcFileReader, createWarcFileReader)
import WarcFileWriter         (WarcFileWriter, createWarcFileWriter)

import           Control.Monad (replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State (StateT, execStateT, modify')
import           Control.Concurrent.STM    (atomically)
import           Data.ByteString.Char8     (ByteString)
import qualified Data.IntSet           as IS
import qualified Data.Set as S
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO Bool
main = do
    systemTests <- execStateT buildAndRunTests []
    mapM_ checkParallel systemTests
    pure True

testEnv :: Monad m => StateT [Group] m Environment
testEnv = do
    let idxPath = "../bin/indexer-qp2"
        colPath = "../collections-test"
        env     = Environment colPath idxPath Nothing
        test1   = ("indexerPath",    property (assert $ indexerBinary env == idxPath))
        test2   = ("collectionsDir", property (assert $ collectionsDir env == colPath))
    modify' ((++) [Group "Environment tests" [test1, test2]])
    pure env

noopLogger :: Monad m => StateT [Group] m (ByteString -> IO ())
noopLogger = pure $ \_ -> pure ()

testedRegistry :: MonadIO m => Environment
                            -> (ByteString -> IO ()) 
                            -> StateT [Group] m Registry
testedRegistry env logger = do
    registry <- liftIO $ createRegistry env logger
    pure registry

testedWarcReader :: MonadIO m => Int
                              -> (ByteString -> IO ())
                              -> StateT [Group] m WarcFileReader
testedWarcReader n l = do
    let warcReader = createWarcFileReader n l
    -- TODO tests
    pure warcReader

testedWarcWriter :: MonadIO m => StateT [Group] m WarcFileWriter
testedWarcWriter = do
    let warcWriter = createWarcFileWriter
    -- TODO tests
    pure warcWriter

testedSnippets :: MonadIO m => WarcFileReader
                            -> StateT [Group] m Snippets
testedSnippets wfr = do
    let snippets = createSnippets wfr
    -- TODO tests
    pure snippets

testedQueryProcessor :: MonadIO m => Environment
                                  -> Registry
                                  -> Snippets
                                  -> (ByteString -> IO ())
                                  -> StateT [Group] m QueryProcessor
testedQueryProcessor env registry snippets logger = do
    let queryProcessor = createQueryProcessor env registry snippets logger
    -- TODO tests
    pure queryProcessor

willReturnToFib :: Property
willReturnToFib = property $ do
    nfc                <- forAll nonFibComponent
    numOtherComponents <- forAll (Gen.int $ Range.constant 0 10)
    fcs                <- forAll (replicateM numOtherComponents component)
    go 0 (S.fromList (nfc:fcs))
    where
    go :: Monad m => Int -> S.Set Component -> PropertyT m ()
    go 1000  _ = error "Too many iterations!"
    go    n cs
        | all (\c -> cmp_size c `IS.member` fibSet) cs = pure ()
        | otherwise =
            case hybridStrategy cs of
                Nothing -> go (n+1) (S.insert (Component 1 (show n)) cs)
                Just (_, a, b) ->
                    let sa = cmp_size a
                        sb = cmp_size b
                        cs' = S.insert (Component (sa + sb) (show n)) . S.delete a . S.delete b $ cs
                    in go (n+1) cs'

    component :: Gen Component
    component = do
        i <- Gen.int $ Range.constant 0 10
        pure $ Component i (show i)

    nonFibComponent :: Gen Component
    nonFibComponent = do
        i <- Gen.filter (\i -> not $ IS.member i fibSet) . Gen.int $ Range.constant 0 200
        pure $ Component i (show i)

testedCompactor :: MonadIO m => Environment
                             -> Registry
                             -> WarcFileWriter
                             -> Snippets
                             -> (ByteString -> IO ())
                             -> StateT [Group] m Compactor
testedCompactor env reg wfw snippets logger = do

    let compactor = createCompactor env reg wfw snippets logger

    modify' ((++) [Group "Compactor" [("will return to fibonacci", willReturnToFib)]])

    pure compactor



testedImporter :: MonadIO m => Environment
                            -> Registry
                            -> Compactor
                            -> StateT [Group] m Importer
testedImporter env reg compactor = do
    let importer = createImporter env reg compactor
    -- TODO tests
    pure importer

testedIndexer :: MonadIO m => Environment
                           -> WarcFileReader
                           -> WarcFileWriter
                           -> Snippets
                           -> Compactor
                           -> Registry
                           -> StateT [Group] m Indexer
testedIndexer env wfr wfw snippets compactor reg = do
    let indexer = createIndexer env wfr wfw snippets compactor reg
    -- TODO tests
    pure indexer
                        
buildTestedEnvironment :: StateT [Group] IO (Environment, Registry, Indexer, QueryProcessor)
buildTestedEnvironment = do
    env        <- testEnv
    logger     <- noopLogger
    registry   <- testedRegistry env logger
    warcReader <- testedWarcReader 128 logger
    warcWriter <- testedWarcWriter
    snippets   <- testedSnippets warcReader
    qp         <- testedQueryProcessor env registry snippets logger
    compactor  <- testedCompactor env registry warcWriter snippets logger
    _          <- testedImporter env registry compactor
    indexer    <- testedIndexer env warcReader warcWriter snippets compactor registry
    pure (env, registry, indexer, qp)

buildAndRunTests :: StateT [Group] IO ()
buildAndRunTests = do
    (env, registry, indexer, queryProcessor) <- buildTestedEnvironment
    testAddTwoDocs env registry indexer
    testSimpleQueries env registry indexer queryProcessor

testAddTwoDocs :: MonadIO m => Environment -> Registry -> Indexer -> m ()
testAddTwoDocs env registry indexer = do
    let colName = "index-2-docs"
    let cn = CollectionName colName
    liftIO $ do
        -- Add two docs
        Right 1 <- indexDocuments indexer cn [Doc "foo-1" "bar-1"]
        1       <- sum . map numDocs . S.toList <$> atomically (viewCollectionComponents registry cn)
        Right 1 <- indexDocuments indexer cn [Doc "foo-2" "bar-2"]
        2       <- sum . map numDocs . S.toList <$> atomically (viewCollectionComponents registry cn)
        -- Cleanup
        atomically $ mapM_ (unregister registry cn) =<< viewCollectionComponents registry cn
        removeDirectoryRecursive (collectionsDir env <> "/" <> colName)

testSimpleQueries :: MonadIO m => Environment -> Registry -> Indexer -> QueryProcessor -> m ()
testSimpleQueries env registry indexer queryProcessor = do
    let colName = "simple-queries"
    let cn = CollectionName colName
    liftIO $ do
        -- Test simple queries
        Right 2 <- indexDocuments indexer cn [ Doc "doc-1" "words in first doc"
                                             , Doc "doc-2" "words in second doc" ]
        Right (QueryResults 0 []) <- runQuery queryProcessor cn $ QueryParams { query = "missing", maxResults = Nothing }
        Right (QueryResults 1  _) <- runQuery queryProcessor cn $ QueryParams { query = "first", maxResults = Nothing }
        Right (QueryResults 1  _) <- runQuery queryProcessor cn $ QueryParams { query = "second", maxResults = Nothing }
        Right (QueryResults 2  _) <- runQuery queryProcessor cn $ QueryParams { query = "words", maxResults = Nothing }
        -- Cleanup
        atomically $ mapM_ (unregister registry cn) =<< viewCollectionComponents registry cn
        removeDirectoryRecursive (collectionsDir env <> "/" <> colName)
