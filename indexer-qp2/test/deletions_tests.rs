#[cfg(test)]
use crate::util::fresh_tmp_dir;
#[cfg(test)]
use indexer_qp2::deletions::*;
#[cfg(test)]
use indexer_qp2::doc::*;
#[cfg(test)]
use indexer_qp2::index::*;
#[cfg(test)]
use indexer_qp2::terms::Term;
#[cfg(test)]
use indexer_qp2::types::*;
#[cfg(test)]
use std::time::SystemTime;

#[cfg(test)]
#[test]
fn test_delete_document_basic() {
    let temp_dir = fresh_tmp_dir("test_delete_basic_");

    let idx_dir = temp_dir.to_str().unwrap().to_string();

    let url = "http://example.com/doc1";

    let docs = vec![test_doc(url)];

    index(&idx_dir, docs, SystemTime::now());

    assert_eq!(is_deleted(&idx_dir, &Url(url.to_string())), "PRESENT");

    delete_document(&idx_dir, &Url(url.to_string()));

    assert_eq!(is_deleted(&idx_dir, &Url(url.to_string())), "DELETED");
}

#[cfg(test)]
#[test]
fn test_delete_document_multiple_docs() {
    let temp_dir = fresh_tmp_dir("test_delete_multiple_");

    let idx_dir = temp_dir.to_str().unwrap().to_string();

    let docs = vec![
        Doc {
            url: Url("http://example.com/doc1".to_string()),
            doc_id: DocId(1),
            term_frequencies: {
                let mut map = std::collections::HashMap::new();
                map.insert(Term("test".to_string()), TermFreq(1));
                map
            },
            doc_len: 1,
        },
        Doc {
            url: Url("http://example.com/doc2".to_string()),
            doc_id: DocId(2),
            term_frequencies: {
                let mut map = std::collections::HashMap::new();
                map.insert(Term("test".to_string()), TermFreq(1));
                map
            },
            doc_len: 1,
        },
        Doc {
            url: Url("http://example.com/doc3".to_string()),
            doc_id: DocId(3),
            term_frequencies: {
                let mut map = std::collections::HashMap::new();
                map.insert(Term("test".to_string()), TermFreq(1));
                map
            },
            doc_len: 1,
        },
    ];

    index(&idx_dir, docs, SystemTime::now());

    let url0 = Url("http://example.com/missing".to_string());
    let url1 = Url("http://example.com/doc1".to_string());
    let url2 = Url("http://example.com/doc2".to_string());
    let url3 = Url("http://example.com/doc3".to_string());

    assert_eq!(is_deleted(&idx_dir, &url0), "MISSING");
    assert_eq!(is_deleted(&idx_dir, &url1), "PRESENT");
    assert_eq!(is_deleted(&idx_dir, &url2), "PRESENT");
    assert_eq!(is_deleted(&idx_dir, &url3), "PRESENT");

    delete_document(&idx_dir, &url1);
    assert_eq!(is_deleted(&idx_dir, &url0), "MISSING");
    assert_eq!(is_deleted(&idx_dir, &url1), "DELETED");
    assert_eq!(is_deleted(&idx_dir, &url2), "PRESENT");
    assert_eq!(is_deleted(&idx_dir, &url3), "PRESENT");

    delete_document(&idx_dir, &url2);
    assert_eq!(is_deleted(&idx_dir, &url0), "MISSING");
    assert_eq!(is_deleted(&idx_dir, &url1), "DELETED");
    assert_eq!(is_deleted(&idx_dir, &url2), "DELETED");
    assert_eq!(is_deleted(&idx_dir, &url3), "PRESENT");

    delete_document(&idx_dir, &url3);
    assert_eq!(is_deleted(&idx_dir, &url0), "MISSING");
    assert_eq!(is_deleted(&idx_dir, &url1), "DELETED");
    assert_eq!(is_deleted(&idx_dir, &url2), "DELETED");
    assert_eq!(is_deleted(&idx_dir, &url3), "DELETED");
}

#[cfg(test)]
#[test]
fn test_delete_document_many_docs() {
    let temp_dir = fresh_tmp_dir("test_delete_all_");

    let idx_dir = temp_dir.to_str().unwrap().to_string();

    let num_docs = 300;

    // Create an index with 8 documents (exactly 1 byte for deletions)
    let mut docs = Vec::new();
    for i in 0..num_docs {
        docs.push(Doc {
            url: Url(format!("http://example.com/doc{}", i)),
            doc_id: DocId(i + 1),
            term_frequencies: {
                let mut map = std::collections::HashMap::new();
                map.insert(Term("test".to_string()), TermFreq(1));
                map
            },
            doc_len: 1,
        });
    }

    index(&idx_dir, docs, SystemTime::now());

    // Delete all documents
    for i in 0..num_docs {
        let url = Url(format!("http://example.com/doc{}", i));
        delete_document(&idx_dir, &url);
    }

    // Verify all are deleted
    for i in 0..num_docs {
        let url = Url(format!("http://example.com/doc{}", i));
        assert_eq!(is_deleted(&idx_dir, &url), "DELETED");
    }
}

#[cfg(test)]
#[test]
fn test_delete_document_idempotent() {
    let temp_dir = fresh_tmp_dir("test_delete_idempotent_");

    let idx_dir = temp_dir.to_str().unwrap().to_string();

    // Create an index with one document
    let docs = vec![Doc {
        url: Url("http://example.com/doc1".to_string()),
        doc_id: DocId(1),
        term_frequencies: {
            let mut map = std::collections::HashMap::new();
            map.insert(Term("test".to_string()), TermFreq(1));
            map
        },
        doc_len: 1,
    }];

    index(&idx_dir, docs, SystemTime::now());

    let url = Url("http://example.com/doc1".to_string());

    // Delete once
    delete_document(&idx_dir, &url);
    assert_eq!(is_deleted(&idx_dir, &url), "DELETED");

    // Delete again (should be idempotent)
    delete_document(&idx_dir, &url);
    assert_eq!(is_deleted(&idx_dir, &url), "DELETED");
}

#[cfg(test)]
#[test]
fn test_delete_and_reindex() {
    let temp_dir = fresh_tmp_dir("test_delete_and_reindex_");

    let idx_dir = temp_dir.to_str().unwrap().to_string();

    let url = Url("http://example.com/zombie".to_string());

    index(
        &idx_dir,
        vec![test_doc("http://example.com/zombie")],
        SystemTime::now(),
    );

    for _ in 0..30 {
        // Delete once
        delete_document(&idx_dir, &url);
        assert_eq!(is_deleted(&idx_dir, &url), "DELETED");

        // Reindex
        index(
            &idx_dir,
            vec![test_doc("http://example.com/zombie")],
            SystemTime::now(),
        );
        assert_eq!(is_deleted(&idx_dir, &url), "PRESENT");
    }
}

fn test_doc(url: &str) -> Doc {
    Doc {
        url: Url(url.to_string()),
        doc_id: DocId(1),
        term_frequencies: {
            let mut map = std::collections::HashMap::new();
            map.insert(Term("test".to_string()), TermFreq(1));
            map
        },
        doc_len: 1,
    }
}
