use bytes::*;
use normalise::*;
use terms::Term;
use types::*;
use write_to_buf::WriteToBuf;

use rand::prelude::*;
use std::collections::HashMap;

pub struct Doc {
    pub url:              Url,
    pub doc_id:           DocId,
    pub term_frequencies: HashMap<Term, TermFreq>,
    pub doc_len:          u32,
}

impl WriteToBuf for Doc {
    fn write_to_buf(&self, buf: &mut Vec<u8>) -> usize {
        write_to_buf(buf, &self.url, &self.doc_id, self.doc_len)
    }
}

#[derive(Debug)]
pub struct DocEntry {
    pub url:     Url,
    pub doc_id:  DocId,
    pub doc_len: u32,
}

impl WriteToBuf for DocEntry {
    fn write_to_buf(&self, buf: &mut Vec<u8>) -> usize {
        write_to_buf(buf, &self.url, &self.doc_id, self.doc_len)
    }
}

fn write_to_buf(buf: &mut Vec<u8>,
                url: &Url,
                doc_id: &DocId,
                doc_len: u32) -> usize {

    let mut written = 0;

    // url
    let Url(u) = &url;
    written += write_esc(buf, u.as_bytes());

    // "\0"
    buf.push(b'\0');
    written += 1; 

    // doc_id
    let DocId(di) = doc_id;
    written += write_esc_u32(buf, *di);

    //doc_len
    written += write_esc_u32(buf, doc_len);

    // newline (not escaped)
    buf.push(b'\n');
    written += 1;

    written
}
  
pub fn mk_doc(url: Url,
              body: &String) -> Doc {

    let mut term_frequencies = HashMap::new();
    let mut doc_len = 0;

    for term in normalise(body) {
        let mut entry = term_frequencies.entry(term).or_insert(TermFreq(0));
        entry += &mut TermFreq(1);
        doc_len += 1;
    }

    Doc {
        url              : url,
        doc_id           : DocId(rand::rng().random()),
        term_frequencies : term_frequencies,
        doc_len          : doc_len,
    }
}
