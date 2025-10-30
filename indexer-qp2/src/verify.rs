use crate::index_reader::*;
use crate::terms::*;
use crate::types::*;
use crate::write_to_buf::WriteToBuf;

use std::fs;

// 1. Term Offsets
fn verify_term_offsets(index_path: &str) {

    let sz =
        fs::metadata(&format!("{}/termOffsets", index_path))
            .expect("Could not get termOffsets filesize").len();

    assert!(
        sz > 0,
        "1.1 Term offsets file must not be empty");

    assert!(
        sz % 8 == 0,
        "1.2 Term offsets file must have size modulo 8: {}", sz);

    with_term_offsets(index_path, &|TermOffsetsRead(offs)| {

        assert!(
            offs[0] == 0,
            "1.3 Term offsets must start from 0");

        let len = offs.len();
        let mut prev = 0;
        for i in 1..len {
            assert!(
                offs[i] > prev,
                "1.4 Term offsets must be strictly increasing");
            prev = offs[i];
        }
    });
}

// 2. Terms
fn verify_terms(index_path: &str) -> u64 {

    let sz =
        fs::metadata(&format!("{}/terms", index_path))
            .expect("Could not get terms filesize").len();

    assert!(
        sz > 0,
        "2.1 Terms file must not be empty");

    with_term_offsets(index_path, &|TermOffsetsRead(offs)|
    with_terms       (index_path, &|terms| {

        let mut doc_freq_sum = 0;

        let mut buf = Vec::<u8>::new();
        let mut written = 0;

        let mut prev = read_term_entry_at(&terms, offs[0] as usize);
        assert!(
            prev.doc_freq > 0,
            "2.3 Doc freqs must be greater than 0");

        doc_freq_sum += prev.doc_freq as u64;
        written += prev.write_to_buf(&mut buf) as u64;

        for term in offs[1..].iter().map(|&o| read_term_entry_at(&terms, o as usize)) {

            assert!(
                term.offset > prev.offset,
                "2.2 Offsets must increase {:?} {:?}", term.offset, prev.offset);

            assert!(
                term.doc_freq > 0,
                "2.3 Doc freqs must be greater than 0");

            assert!(
                term.term > prev.term,
                "2.4 Terms must be ordered alphabetically {:?} {:?}", term.term, prev.term);

            doc_freq_sum += prev.doc_freq as u64;
            written += term.write_to_buf(&mut buf) as u64;

            prev = term;
        }

        assert!(
            written == sz,
            "2.5 Terms must fill the file fully {}/{}", written, sz);

        doc_freq_sum
    }))
}

// 3. Postings

fn verify_postings(index_path: &str) {

    let sz = fs::metadata(&format!("{}/postings", index_path))
                .expect("Could not get terms filesize").len();

    assert!(
        sz > 0,
        "3.1 Postings file must not be empty");

    assert!(
        sz % 8 == 0,
        "3.2 Postings offsets file must have size modulo 8: {}", sz);

    with_term_offsets(index_path, &|TermOffsetsRead(offs)|
    with_terms       (index_path, &|terms| 
    with_postings    (index_path, &|PostingsRead(posts)| {

        for term in offs.iter().map(|&o| read_term_entry_at(&terms, o as usize)) {

            let start =         term.offset   as usize;
            let end   = start + term.doc_freq as usize;

            let mut prev = posts[start*2];
            for p in start+1..end {
                let post = posts[2 * p];
                assert!(prev < post,
                    "3.3 Individual Postings runs must strictly increase {:?}: {} {}\n failure in {}", term, prev, post, index_path);
                prev = post;
            }
        }
    })));

    // "3.4 Terms' docfreqs must sum to total number of postings"
}

// Every set <posting> must == set >doc.docid>

pub fn verify(index_path: &str) {

    verify_term_offsets(index_path);

    // doc_freq_sum should equal postings length
    let _doc_freq_sum = verify_terms(index_path);

    verify_postings(index_path);


    /*
    let &TermOffsetsRead(term_offs) = ir.term_offsets;
    for &off in term_offs {
        let TermEntry { term        // TODO strictly increasing
                      , offset: _     // TODO strictly increasing
                      , doc_freq: _    // TODO 0 < doc_freq <= num_docs
                      } = read_term_entry_at(ir.terms, off as usize);
       // println!("Offset {}: {:?}", off, term);
    }

    let &DocOffsetsRead(doc_offs) = ir.doc_offsets;

    for &off in doc_offs {

        let DocEntry { url      // TODO non-empty, ASCII?
                     , doc_id   // TODO strictly increasing, != 0
                     , doc_len  // TODO 0 < doc_len <= total_doc_len
                                // TODO sum(doc_len) == total_doc_len
                     } = read_doc_at(ir.docs, off as usize);
       // println!("Offset {}: {:?} {:?} {}", off, url, doc_id, doc_len);
    }
*/
    // TODO all single-term queries should cover all doc_ids

    // TODO Set(posting docids) == Set(docs docids)
}