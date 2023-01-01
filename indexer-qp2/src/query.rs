
use index_reader::*;
use input::*;
use ranking::*;
use terms::*;
use types::*;

use itertools::*;
use serde::Serialize;   

#[derive(Debug, Serialize)]
pub struct QueryResult { pub num_results: usize
                       , pub results:     Vec<Scored>
                       }

pub fn query(ir:           &IndexRead,
             query_params: &QueryParams) -> QueryResult {

    let mut scored = Vec::new();

    run_query(&mut scored,
              ir,
              &query_params);

    QueryResult { num_results: scored.len()
                , results:     scored
                }
}

fn run_query(out_scored:   &mut Vec<Scored>,
             ir:           &IndexRead,        
             query_params: &QueryParams) -> () {

    let terms: Vec<TermEntry> =
        query_params.query_terms
                    .iter()

                    // Look up the string terms into term ids
                    .map    ( | s| find_term(ir, s) )

                    // Keep only the found term ids
                    .filter ( |rt| rt.is_some() )
                    .map    ( |rt| rt.unwrap() )
                    .collect();

    let scored_iter =
        scored_iterator(ir, &terms, query_params);

    match query_params.max_results {

        None => {
            for scored in scored_iter {
                out_scored.push(scored);
            }
        },

        Some(mr) => {

            use std::cmp::Reverse;
            use std::collections::BinaryHeap;

            let mut min_score_heap: BinaryHeap<Reverse<Scored>> = BinaryHeap::new();
            let mut score_heap_sz: u32 = 0;

            for scored in scored_iter {

                if score_heap_sz < mr {
                    // If we don't have enough docs yet, any result is good enough
                    score_heap_sz = score_heap_sz + 1;
                    min_score_heap.push(Reverse(scored));
                } else {
                    // Too many results, start comparing!
                    let Reverse(min) = min_score_heap.peek().unwrap();
                    if scored > *min {
                        min_score_heap.pop();
                        min_score_heap.push(Reverse(scored));
                    }
                }
            }
            for Reverse(v) in min_score_heap {
                out_scored.push(v);
            }
        }
    }
}

// TODO be able to score here while still streaming
// So as to decide whether to include low-term-matched docs
fn scored_iterator<'a>(ir:           &'a IndexRead,
                       terms:        &'a Vec<TermEntry>,
                       query_params: &'a QueryParams) -> impl Iterator <Item=Scored> + 'a {

    use std::collections::HashMap;

    let num_query_terms = query_params.query_terms.len();

    let doc_freqs: HashMap<&Term, u32> =
            terms.iter().map(|t| (&t.term, t.doc_freq)).collect();

    terms.iter()
         .map(|t| postings_list_for_term(ir.postings, t))
         .kmerge_by(|a,b| a.doc_id <= b.doc_id) 
         .coalesce(|a,b| {
             // TODO can neaten
             if a.doc_id == b.doc_id {
                 let mut term_freqs = Vec::new();
                 for term_freq in &a.term_freqs {
                     term_freqs.push(term_freq.clone());
                 }
                 for term_freq in &b.term_freqs {
                     term_freqs.push(term_freq.clone());
                 }
                 Ok(TermPost { doc_id:     a.doc_id
                             , term_freqs: term_freqs
                             })
                 } else {
                     Err((a,b))
                 }})
         .filter(move |xs| xs.term_freqs.len() == num_query_terms)
         .map(move |xs| rank_result(ir, &doc_freqs, xs))
}

fn postings_list_for_term<'a>(PostingsRead(posts): &'a PostingsRead,
                              term:                &'a TermEntry) -> impl Iterator <Item=TermPost<'a>> + 'a {
    let start =         term.offset   as usize;
    let end   = start + term.doc_freq as usize;
    (start..end).map(move |i| TermPost { doc_id:     posts[2 * i]
                                       , term_freqs: vec!((&term.term, posts[2 * i + 1]))
                                       })
}
