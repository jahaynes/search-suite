extern crate byteorder;
extern crate edit_distance;
extern crate flate2;
extern crate intmap;
extern crate itertools;
extern crate memmap;
extern crate num;
extern crate rand;
extern crate serde;
extern crate uuid;

mod bk_tree;
mod bytes;
mod deletions;
mod doc;
mod dump;
mod index;
mod index_reader;
mod index_writer;
mod input;
mod merge;
mod normalise;
mod query;
mod ranking;
mod spelling_correction;
mod terms;
mod types;
mod verify;
mod write_to_buf;

#[cfg(test)]
mod byte_tests;
#[cfg(test)]
mod system_tests;

use deletions::*;
use dump::*;
use index::*;
use index_reader::*;
use input::*;
use query::*;
use spelling_correction::*;
use types::*;
use verify::*;

use std::collections::HashMap;
use std::env;

fn main() {

  let args: Vec<String> = env::args().collect();

  match args[1].as_str() {

    "dump"       => dump(&args[2]),

    "index_json" => { let input_docs = docs_from_stdin();

                      let result: IndexResult;

                      if !input_docs.is_empty() {
                        result = index(&args[2], input_docs);
                      } else {
                        result = IndexResult { num_docs: 0, num_terms: 0 };
                      }
                      // verify(&args[2]);
                      let serialized = serde_json::to_string(&result).unwrap();
                      println!("{}", serialized);
                    },

    "merge"     => { let dest          = &args[2];
                     let src1          = &args[3];
                     let src2          = &args[4];
                     merge::merge::merge(dest, src1, src2);
                     // verify(dest);
                   },

    "num_docs"   => { let idx_name   = &args[2];
                      let num_docs   = with_doc_offsets(idx_name, &|DocOffsetsRead(offs)| offs.len() as u32);
                      let serialized = serde_json::to_string(&NumDocs{num_docs: num_docs}).unwrap();
                      println!("{}", serialized);
                    },

    "query"      => { let query_params = parse_query_params(&args);
                      with_index_read(&query_params.base_path, &|ir| {
                          let results    = query(&ir, &query_params);
                          let serialized = serde_json::to_string(&results).unwrap();
                          println!("{}", serialized);
                    })},

    "spelling"   => { let idx_name     = &args[2];
                      let str_max_dist = &args[3];
                      let max_dist     = str_max_dist.parse::<i64>().expect("max_dist should be an i64");
                      let mut results  = HashMap::new();
                      args[4..].iter()
                               .map(|term| term.to_lowercase())
                               .for_each(|term| {
                                  let near_matches = query_bk(&idx_name, &term, max_dist);
                                  if !near_matches.is_empty() {
                                    results.insert(term, near_matches);
                                  }});
                      let serialized = serde_json::to_string(&results).unwrap();
                      print!("{}", serialized);
                    },

    "delete_doc" => { let idx_name   = &args[2];
                      let str_url    = &args[3];
                      delete_document(idx_name, &Url(String::from(str_url)));
                    },

    "is_deleted" => { let idx_dir = &args[2];
                      let str_url = &args[3];
                      let url     = Url(String::from(str_url));
                      let deleted = is_deleted(idx_dir, &url);
                      print!("{}", deleted);
                    },

    "doc_url_exists" => { let idx_name   = &args[2];
                          let str_url    = &args[3];
                          let exists     = doc_url_exists(idx_name, &Url(String::from(str_url)));
                          let serialized = serde_json::to_string(&exists).unwrap();
                          println!("{}", serialized);
                        },

    "verify"     => verify(&args[2]),

    _            => panic!("usage")
  }
}
