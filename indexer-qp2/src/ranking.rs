use crate::index_reader::*;
use crate::terms::*;
use crate::types::*;

use serde::Serialize;
use std::cmp::Ordering;
use std::collections::HashMap;

#[derive(Clone, Debug, Serialize)]
pub struct Scored {
    pub uri: String,
    pub score: f64,
    pub term_count: u32,
}

#[derive(Debug)]
pub struct TermPost<'a> {
    pub doc_id: u32,
    pub term_freqs: Vec<(&'a Term, u32)>,
}

impl PartialEq for Scored {
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score
    }
}
impl Eq for Scored {}
impl PartialOrd for Scored {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Scored {
    /* Converts floats to integers to make ordering possible
    Raising by 10000 is hopefully enough to preserve significance! */
    fn cmp(&self, other: &Self) -> Ordering {
        let my_score = (10000.0 * self.score.floor()) as i64;
        let other_score = (10000.0 * other.score.floor()) as i64;
        my_score.cmp(&other_score)
    }
}

pub fn rank_result_bm25(
    ir: &IndexRead,
    doc_freqs: &HashMap<&Term, u32>,
    TermPost { doc_id, term_freqs }: TermPost,
) -> Scored {
    let DocOffsetsRead(doc_offsets) = ir.doc_offsets;
    let num_docs = doc_offsets.len() as u64;
    let avg_doc_len = (ir.total_doc_len / num_docs) as u32;

    match find_doc(ir, DocId(doc_id)) {
        None => panic!("shouldn't happen (missing docid {})", doc_id),

        Some(doc) => {
            let mut doc_score = 0.0;
            let mut term_count = 0;

            for (term, term_freq) in term_freqs {
                let doc_freq = match doc_freqs.get(term) {
                    Some(doc_freq) => doc_freq,
                    None => panic!("Missing doc_freq"),
                };

                let score =
                    idf(num_docs, *doc_freq as f64) * bm25(avg_doc_len, doc.doc_len, term_freq);

                doc_score += score;
                term_count += 1;
            }

            let Url(uri) = doc.url;

            Scored {
                uri: uri,
                score: doc_score,
                term_count: term_count,
            }
        }
    }
}

fn bm25(av_doc_len: u32, doc_len: u32, term_freq: u32) -> f64 {
    let top = term_freq as f64 * (1.2 + 1.0);
    let bottom =
        term_freq as f64 + 1.2 * (1.0 - 0.75 + 0.75 * (doc_len as f64 / av_doc_len as f64));
    top / bottom
}

fn idf(total_num_docs: u64, doc_freq: f64) -> f64 {
    let top = total_num_docs as f64 - doc_freq + 0.5;
    let bot = doc_freq + 0.5;
    (top / bot).ln()
}
