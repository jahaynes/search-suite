use crate::doc::*;
use crate::index_writer::*;
use crate::terms::*;
use crate::types::*;
use crate::write_to_buf::WriteToBuf;

use serde::Serialize;
use std::collections::HashMap;
use std::time::SystemTime;

struct Inverted(Vec<(Term, Vec<(DocId, TermFreq)>)>);

#[derive(Serialize)]
pub struct IndexResult { pub num_docs:  usize
                       , pub num_terms: u32
                       , pub ms_taken:  u128
                       }

pub struct Index<'a> { pub total_doc_len: u64 
                     , pub term_offsets:  &'a TermOffsets
                     , pub terms:         &'a Terms
                     , pub postings:      &'a Postings
                     , pub doc_offsets:   &'a DocOffsets
                     , pub docs:          &'a Docs
                     }

pub fn index(idx_dir: &String,
             mut mem_docs: Vec<Doc>,
             start_time: SystemTime) -> IndexResult {

    let total_doc_len =
            mem_docs.iter()
                    .map(|doc| doc.doc_len as u64)
                    .sum();

    let num_docs =
            mem_docs.len();

    mem_docs.sort_by(|a, b| a.doc_id.cmp(&b.doc_id));

    let (docs, doc_offsets) =
            mk_docs(&mem_docs);

    let inverted =
            mk_inverted(mem_docs);

    let (terms, num_terms, term_offsets) =
            mk_terms(&inverted);

    let postings =
            mk_postings(&inverted);

    // TODO reject more harshly?
    if num_docs != 0 && num_terms != 0 {

        let idx = Index {
            total_doc_len: total_doc_len,
            term_offsets:  &term_offsets,
            terms:         &terms,
            postings:      &postings,
            doc_offsets:   &doc_offsets,
            docs:          &docs
        };

        write_files(idx_dir, idx);
        let ms_taken = SystemTime::now().duration_since(start_time).unwrap().as_millis();
        IndexResult { num_docs: num_docs, num_terms: num_terms, ms_taken: ms_taken }
    } else {
        let ms_taken = SystemTime::now().duration_since(start_time).unwrap().as_millis();
        IndexResult { num_docs: 0, num_terms: 0, ms_taken: ms_taken }
    }
}

fn mk_docs(mem_docs: &Vec<Doc>) -> (Docs, DocOffsets) {

    let mut buf = Vec::<u8>::new();
    let mut offset = 0;
    let mut offsets = Vec::<u64>::new();

    for doc in mem_docs {
        let written = doc.write_to_buf(&mut buf);
        offsets.push(offset);
        offset += written as u64;
    }

    ( Docs(Escaped(buf))
    , DocOffsets(offsets)
    )
}

fn mk_postings(Inverted(postings): &Inverted) -> Postings {
    
    let mut buf = Vec::<u32>::new();
    for (_, posts) in postings {
        for (DocId(di), TermFreq(tf)) in posts {
            buf.push(*di);
            buf.push(*tf);
        }
    }
    Postings(buf)
}

fn mk_terms(Inverted(postings): &Inverted) -> (Terms, u32, TermOffsets) {

    // Terms
    let mut buf = Vec::<u8>::new();
    let mut doc_freqs = 0;

    // Num Terms
    let mut num_terms = 0;

    // Term Offsets
    let mut offsets = Vec::<u64>::new();
    let mut offset: u64 = 0;

        // OK? this term is copied below using Term(term.to_string())
    for (Term(term), post) in postings {

        // Terms
        let doc_freq = post.len() as u32;
        let written = TermEntry { term:     Term(term.to_string())
                                , offset:   doc_freqs
                                , doc_freq: doc_freq
                                }.write_to_buf(&mut buf);
        doc_freqs += doc_freq as u64;

        // Num Terms
        num_terms += 1;

        // Term Offsets
        //offsets.extend(&u64_as_u8_arr(offset));
        offsets.push(offset);
        offset += written as u64;
    }

    ( Terms(Escaped(buf))
    , num_terms
    , TermOffsets(offsets)
    )
}

fn mk_inverted(mem_docs: Vec<Doc>) -> Inverted {
    let mut postings_map = HashMap::new();
    for doc in mem_docs {
        for (term, term_freq) in doc.term_frequencies {
            let entry = postings_map.entry(term).or_insert(Vec::new());
            entry.push((doc.doc_id, term_freq));
        }
    }

    let mut postings = Vec::new();
    for (term, posts) in postings_map {
        postings.push((term, posts));
    }

    postings.sort();

    Inverted(postings)
}