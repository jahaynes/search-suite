use index_reader::*;
use input::*;
use terms::*;
use types::*;

use intmap::IntMap;
use serde::Serialize;
use std::cmp::Ordering;
use std::cmp::Reverse;
use std::collections::BinaryHeap;

#[derive(Debug)]
pub struct Posting {
    pub doc_id: u32,
    pub term_freq: u32,
}

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

use std::collections::HashMap;

pub fn rank_result(
    ir: &IndexRead,
    doc_freqs: &HashMap<&Term, u32>,
    TermPost { doc_id, term_freqs }: TermPost,
) -> Scored {
    let DocOffsetsRead(doc_offsets) = ir.doc_offsets;
    let num_docs = doc_offsets.len() as u64;
    let avg_doc_len = ir.total_doc_len as u32 / num_docs as u32;

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

pub fn _rank_results(
    out_scored: &mut Vec<Scored>,
    ir: &IndexRead,
    query_params: &QueryParams,
    results: &Vec<(TermEntry, Vec<Posting>)>,
) -> () {
    let DocOffsetsRead(doc_offsets) = ir.doc_offsets;

    let num_docs = doc_offsets.len() as u64;
    let avg_doc_len = ir.total_doc_len as u32 / num_docs as u32;

    // Keyed on doc_id
    let mut m: IntMap<u32, Scored> = IntMap::new();

    // Step 1 - aggregate them all
    for (term, postings) in results {
        for p in postings {
            // This is pretty inefficient (binary search of large file inside tight loop)
            match find_doc(ir, DocId(p.doc_id)) {
                None => panic!(
                    "shouldn't happen (missing docid {} from {})",
                    p.doc_id, query_params.base_path
                ),

                Some(doc) => {
                    // 'remove' is used instead of 'get' because 'insert' (below) does not overwrite
                    let old_score = m.remove(p.doc_id.into()).unwrap_or(Scored {
                        uri: "".to_string(),
                        score: 0.0,
                        term_count: 0,
                    });
                    let new_score = idf(num_docs, term.doc_freq as f64)
                        * bm25(avg_doc_len, doc.doc_len, p.term_freq);

                    let Url(uri) = doc.url;
                    let new_scored = Scored {
                        uri: uri,
                        score: old_score.score + new_score,
                        term_count: old_score.term_count + 1,
                    };

                    // warn - insert does not seem to overwrite
                    m.insert(p.doc_id.into(), new_scored);
                }
            }
        }
    }

    match query_params.max_results {
        // Step 2 - keep all docs
        None => {
            for v in m.values() {
                out_scored.push(v.to_owned());
            }
        }

        // Step 2 - keep only the top num_docs
        Some(mr) => {
            let mut min_score_heap: BinaryHeap<Reverse<&Scored>> = BinaryHeap::new();
            let mut score_heap_sz: u32 = 0;
            for scored in m.values() {
                // If we don't have enough docs yet, any result is good enough
                if score_heap_sz < mr {
                    score_heap_sz = score_heap_sz + 1;
                    min_score_heap.push(Reverse(&scored));
                } else {
                    // Too many results, start comparing!
                    let &Reverse(min) = min_score_heap.peek().unwrap();
                    if scored > min {
                        min_score_heap.pop();
                        min_score_heap.push(Reverse(&scored));
                    }
                }
            }

            for Reverse(v) in min_score_heap {
                out_scored.push(v.to_owned());
            }
        }
    };
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
