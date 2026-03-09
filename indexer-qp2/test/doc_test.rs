#[cfg(test)]
use indexer_qp2::doc::{mk_doc, DocEntry};
#[cfg(test)]
use indexer_qp2::types::{DocId, Url};
#[cfg(test)]
use indexer_qp2::write_to_buf::WriteToBuf;
#[cfg(test)]
use indexer_qp2::index_reader::read_doc_at;
#[cfg(test)]
use indexer_qp2::types::DocsRead;

#[cfg(test)]
#[test]
fn test_mk_doc() {
    let url  = "https://example.com/test";
    let body = "Hello world test document".to_string();

    let doc = mk_doc(Url(url.to_string()), &body, || 2345);

    assert_eq!(url, doc.url.0);
    assert_eq!(8, doc.doc_len);
    assert!(!doc.term_frequencies.is_empty());
}

#[cfg(test)]
#[test]
fn test_doc_write_to_buf() {
    let url = Url("https://example.com/test".to_string());
    let body = "Hello world".to_string();

    let doc = mk_doc(url, &body, || 1234);
    let mut buf = Vec::<u8>::new();
    let written = doc.write_to_buf(&mut buf);
    assert_eq!(34, written);

    let read_doc = read_doc_at(&DocsRead(&buf), 0);
    assert_eq!(doc.url.0, read_doc.url.0);
    assert_eq!(doc.doc_id, read_doc.doc_id);
    assert_eq!(doc.doc_len, read_doc.doc_len);
}

#[cfg(test)]
#[test]
fn test_doc_entry_write_to_buf() {
    let url = Url("https://example.com/test".to_string());
    let doc_entry = DocEntry {
        url,
        doc_id: DocId(42),
        doc_len: 100,
    };

    let mut buf = Vec::<u8>::new();
    let written = doc_entry.write_to_buf(&mut buf);
    assert_eq!(34, written);

    let read_doc = read_doc_at(&DocsRead(&buf), 0);
    assert_eq!(doc_entry.url.0, read_doc.url.0);
    assert_eq!(doc_entry.doc_id, read_doc.doc_id);
    assert_eq!(doc_entry.doc_len, read_doc.doc_len);
}
