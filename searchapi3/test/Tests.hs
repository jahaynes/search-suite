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
import Metadata               (MetadataApi, createMetadataApi)
import System.Directory       (removeDirectoryRecursive)
import Types                  (CollectionName (..), numDocs)
import WarcFileReader         (WarcFileReader, createWarcFileReader)
import WarcFileWriter         (WarcFileWriter, createWarcFileWriter)

import           Control.Monad               (replicateM, unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State   (StateT, execStateT, modify')
import           Control.Concurrent.STM      (atomically)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.IntSet           as IS
import qualified Data.Map as M
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

testedMetadataApi :: MonadIO m => WarcFileReader
                               -> StateT [Group] m MetadataApi
testedMetadataApi wfr = do
    let metadataApi = createMetadataApi wfr
    -- TODO tests
    pure metadataApi

testedQueryProcessor :: MonadIO m => Environment
                                  -> Registry
                                  -> MetadataApi
                                  -> (ByteString -> IO ())
                                  -> StateT [Group] m QueryProcessor
testedQueryProcessor env registry metadataApi logger = do
    let queryProcessor = createQueryProcessor env registry metadataApi logger
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
                             -> MetadataApi
                             -> (ByteString -> IO ())
                             -> StateT [Group] m Compactor
testedCompactor env reg wfw metadataApi logger = do

    let compactor = createCompactor env reg wfw metadataApi logger

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
                           -> MetadataApi
                           -> Compactor
                           -> Registry
                           -> StateT [Group] m Indexer
testedIndexer env wfr wfw metadataApi compactor reg = do
    let indexer = createIndexer env wfr wfw metadataApi compactor reg
    -- TODO tests
    pure indexer
                        
buildTestedEnvironment :: StateT [Group] IO (Environment, Registry, Indexer, QueryProcessor)
buildTestedEnvironment = do
    env         <- testEnv
    logger      <- noopLogger
    registry    <- testedRegistry env logger
    warcReader  <- testedWarcReader 128 logger
    warcWriter  <- testedWarcWriter
    metadataApi <- testedMetadataApi warcReader
    qp          <- testedQueryProcessor env registry metadataApi logger
    compactor   <- testedCompactor env registry warcWriter metadataApi logger
    _           <- testedImporter env registry compactor
    indexer     <- testedIndexer env warcReader warcWriter metadataApi compactor registry
    pure (env, registry, indexer, qp)

buildAndRunTests :: StateT [Group] IO ()
buildAndRunTests = do
    (env, registry, indexer, queryProcessor) <- buildTestedEnvironment
    testAddTwoDocs env registry indexer
    testSimpleQueries env registry indexer queryProcessor
    testDocumentDeletion env registry indexer queryProcessor
    testMultiIndexDeletion env registry indexer
    testDeletionAndMerge env registry indexer
    testDeleteAndReAdd indexer queryProcessor

testAddTwoDocs :: MonadIO m => Environment -> Registry -> Indexer -> m ()
testAddTwoDocs env registry indexer = do
    let colName = "index-2-docs"
    let cn = CollectionName colName
    liftIO $ do

        -- Add two docs
        indexDocuments indexer cn [Doc "foo-1" "bar-1"] >>= \r ->
            assertR "Indexed 1 document" id 1 r

        sum . map numDocs . S.toList <$> atomically (viewCollectionComponents registry cn) >>= \r ->
            assertR "Expected 1 doc in total" id 1 (Right r)

        indexDocuments indexer cn [Doc "foo-2" "bar-2"] >>= \r ->
            assertR "Indexed 1 document" id 1 r

        sum . map numDocs . S.toList <$> atomically (viewCollectionComponents registry cn) >>= \r ->
            assertR "Expected 2 docs in total" id 2 (Right r)

        -- Cleanup
        atomically $ mapM_ (unregister registry cn) =<< viewCollectionComponents registry cn
        removeDirectoryRecursive (collectionsDir env <> "/" <> colName)

testSimpleQueries :: MonadIO m => Environment -> Registry -> Indexer -> QueryProcessor -> m ()
testSimpleQueries env registry indexer queryProcessor = do
    let colName = "simple-queries"
        cn = CollectionName colName
    liftIO $ do

        -- Test simple queries
        indexDocuments indexer cn [ Doc "doc-1" "words in first doc"
                                  , Doc "doc-2" "words in second doc" ] >>= \r ->
            assertR "Indexed 2 documents" id 2 r

        runQuery queryProcessor cn (QueryParams { query = "missing", maxResults = Nothing }) >>= \r -> do
            assertR "Expected 0 results" num_results 0 r
            assertR "Expected 0 results" (null . results) True r

        runQuery queryProcessor cn (QueryParams { query = "first", maxResults = Nothing }) >>= \r ->
            assertR "Expected 1 result" num_results 1 r

        runQuery queryProcessor cn (QueryParams { query = "second", maxResults = Nothing }) >>= \r ->
            assertR "Expected 1 result" num_results 1 r

        runQuery queryProcessor cn (QueryParams { query = "words", maxResults = Nothing }) >>= \r ->
            assertR "Expected 2 results" num_results 2 r

        -- Cleanup
        atomically $ mapM_ (unregister registry cn) =<< viewCollectionComponents registry cn
        removeDirectoryRecursive (collectionsDir env <> "/" <> colName)

testDocumentDeletion :: MonadIO m => Environment -> Registry -> Indexer -> QueryProcessor -> m ()
testDocumentDeletion env registry indexer queryProcessor = do
    let colName = "doc-deletion"
        cn = CollectionName colName
    liftIO $ do

        -- Make three documents
        indexDocuments indexer cn [ Doc "doc-1" "words in first doc"
                                  , Doc "doc-2" "words in second doc"
                                  , Doc "doc-3" "this is doc 3" ] >>= \r ->
            assertR "Indexed 3 documents" id 3 r

        -- Ensure query returns all three
        runQuery queryProcessor cn (QueryParams { query = "doc", maxResults = Nothing }) >>= \r ->
            assertR "Return 3 documents" num_results 3 r

        -- Ensure document 'is present'
        isDocDeleted indexer cn "doc-2" >>= \r ->
            assertR "doc-2 is present" id (M.singleton "PRESENT" 1) r

        -- Delete a document
        deleteDocument indexer cn "doc-2" >>= \r ->
            assertR "Document deleted" id () r

        -- Ensure the document 'is deleted'
        isDocDeleted indexer cn "doc-2" >>= \r ->
            assertR "doc-2 is deleted" id (M.singleton "DELETED" 1) r

        -- Ensure document is not returned in query
        runQuery queryProcessor cn (QueryParams { query = "second", maxResults = Nothing }) >>= \r -> do
            assertR "Must not return doc-2 anymore" num_results 0 r
            assertR "Must not return doc-2 anymore" (null . results) True r

        -- Ensure query returns only 2 docs
        runQuery queryProcessor cn (QueryParams { query = "doc", maxResults = Nothing }) >>= \r ->
            assertR "Return 2 documents" num_results 2 r

        -- Cleanup
        atomically $ mapM_ (unregister registry cn) =<< viewCollectionComponents registry cn
        removeDirectoryRecursive (collectionsDir env <> "/" <> colName)

testMultiIndexDeletion :: MonadIO m => Environment -> Registry -> Indexer -> m ()
testMultiIndexDeletion env registry indexer = do
    let colName = "multi-index-doc-deletion"
        cn = CollectionName colName
    liftIO $ do

        let doc1 = Doc "doc-1" "words in first doc"
            doc2 = Doc "doc-2" "words in second doc"
            doc3 = Doc "doc-3" "this is doc 3"

        -- Index three documents
        indexDocuments indexer cn [doc1, doc2, doc3] >>= \r ->
            assertR "Indexed 3 documents" id 3 r

        -- The three docs should comprise one index
        countCollectionsIO registry cn >>= \n ->
            assertR "One index" id 1 n

        -- Index an already indexed document
        indexDocuments indexer cn [doc1] >>= \r ->
            assertR "Indexed 1 document" id 1 r

        -- The fourth doc should comprise an extra index
        countCollectionsIO registry cn >>= \n ->
            assertR "Two indices" id 2 n

        -- Ensure document 'is present' in both indexes
        isDocDeleted indexer cn "doc-1" >>= \r ->
            assertR "doc-1 is present" id (M.singleton "PRESENT" 2) r

        -- Delete a document
        deleteDocument indexer cn "doc-1" >>= \r ->
            assertR "Document deleted" id () r

        -- Ensure the document 'is deleted' in both indexes
        isDocDeleted indexer cn "doc-1" >>= \r ->
            assertR "doc-1 is deleted" id (M.singleton "DELETED" 2) r

        -- Cleanup
        atomically $ mapM_ (unregister registry cn) =<< viewCollectionComponents registry cn
        removeDirectoryRecursive (collectionsDir env <> "/" <> colName)

-- TODO delete then re-add?

testDeletionAndMerge :: MonadIO m => Environment -> Registry -> Indexer -> m ()
testDeletionAndMerge env registry indexer = do
    let colName = "doc-deletion-then-merge"
        cn = CollectionName colName
    liftIO $ do

        let doc1 = Doc "doc-1" "words in first doc"
            doc2 = Doc "doc-2" "words in second doc"
            doc3 = Doc "doc-3" "this is doc 3"

        -- Index two documents
        indexDocuments indexer cn [doc1, doc2] >>= \r ->
            assertR "Indexed 2 documents" id 2 r

        -- The two docs should comprise one index
        countCollectionsIO registry cn >>= \n ->
            assertR "One index" id 1 n

        -- Delete a document
        deleteDocument indexer cn "doc-1" >>= \r ->
            assertR "Document deleted" id () r

        -- Ensure the document 'is deleted'
        isDocDeleted indexer cn "doc-1" >>= \r ->
            assertR "doc-1 is deleted" id (M.singleton "DELETED" 1) r

        -- Index a third document
        indexDocuments indexer cn [doc3] >>= \r ->
            assertR "Indexed 1 document" id 1 r

        -- The third doc should cause a merge (into 1 index)
        countCollectionsIO registry cn >>= \n ->
            assertR "One index" id 1 n

        -- Ensure the document 'is missing'
        isDocDeleted indexer cn "doc-1" >>= \r ->
            assertR "doc-1 is missing" id (M.singleton "MISSING" 1) r

        -- Cleanup
        atomically $ mapM_ (unregister registry cn) =<< viewCollectionComponents registry cn
        removeDirectoryRecursive (collectionsDir env <> "/" <> colName)

testDeleteAndReAdd :: MonadIO m => Indexer -> QueryProcessor -> m ()
testDeleteAndReAdd indexer queryProcessor =
    let colName = "doc-delete-and-readd"
        cn      = CollectionName colName
        doc1    = Doc "doc-1" "words in first doc"
        doc2    = Doc "doc-2" "words in second doc"
        doc3    = Doc "doc-3" "this is doc 3"

    in liftIO $ do

        -- Index three documents
        indexDocuments indexer cn [doc1, doc2, doc3] >>= \r ->
            assertR "Indexed 3 documents" id 3 r

        -- Delete doc-2
        deleteDocument indexer cn "doc-2" >>= \r ->
            assertR "Document deleted" id () r

        -- Ensure doc-2 'is deleted'
        isDocDeleted indexer cn "doc-2" >>= \r ->
            assertR "doc-2 is deleted" id (M.singleton "DELETED" 1) r

        -- It should return no results
        runQuery queryProcessor cn (QueryParams { query = "second", maxResults = Nothing }) >>= \r ->
            assertR "Expected 0 results" num_results 0 r

        -- Index doc-2 again
        indexDocuments indexer cn [doc2] >>= \r ->
            assertR "Indexed 1 documents" id 1 r

        -- It should return a result
        runQuery queryProcessor cn (QueryParams { query = "second", maxResults = Nothing }) >>= \r ->
            assertR "Expected 1 results" num_results 1 r

countCollectionsIO :: Registry -> CollectionName -> IO (Either String Int)
countCollectionsIO reg cn = atomically (Right . S.size <$> viewCollectionComponents reg cn)

assertR :: (Applicative f, Eq a, Show a) => String
                                         -> (b -> a)
                                         -> a 
                                         -> Either String b -> f ()
assertR   _ _ _ (Left l)  = error l
assertR msg f y (Right x) =
    let y' = f x in
    unless (y' == y) . error $ unlines [ "\nExpected:"
                                       , "  " ++ show y
                                       , "Actual:"
                                       , "  " ++ show y'
                                       , "in:"
                                       , "  " ++ msg ]