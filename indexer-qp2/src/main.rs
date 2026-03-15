extern crate edit_distance;

use indexer_qp2::deletions::*;
use indexer_qp2::dump::*;
use indexer_qp2::index::*;
use indexer_qp2::index_reader::*;
use indexer_qp2::merge::*;
use indexer_qp2::input::*;
use indexer_qp2::query::*;
use indexer_qp2::spelling_correction::*;
use indexer_qp2::types::*;
use indexer_qp2::verify::*;

use shared_proto::protocol::encode::Cbor;
use shared_proto::protocol::types::IndexReply;

use std::collections::HashMap;
use std::env;
use std::io::Write;
use std::time::SystemTime;

fn main() {

  let start_time = SystemTime::now();

  let args: Vec<String> = env::args().collect();

  match args[1].as_str() {

    "dump"       => dump(&args[2]),

    "doc"        => { let doc_id    = args[2].parse::<u32>().expect("doc_id should be a u32");
                      let base_path = &args[3];
                      with_index_read(base_path, &|ir| {
                          match find_doc(&ir, DocId(doc_id)) {
                              None => {},
                              Some(doc) => {
                                  let serialized = serde_json::to_string(&doc).unwrap();
                                  println!("{}", serialized);
                              }
                          }
                      })
                    },

    "index_docs" => { let input_docs = docs_from_stdin_cbor();
                      let result: IndexResult = index(&args[2], input_docs, start_time);
                      let cbor_result = IndexReply {
                            num_docs:  result.num_docs as u32,
                            num_terms: result.num_terms as u32,
                            ms_taken:  result.ms_taken as u64
                      };
                      let cbor_bytes = cbor_result.cbor().expect("CBOR fail");
                      let stdout = std::io::stdout();
                      stdout.lock().write_all(&cbor_bytes).expect("Failed to write to stdout");
                    }

    "merge"     => { let dest          = &args[2];
                     let src1          = &args[3];
                     let src2          = &args[4];
                     merge::merge(dest, src1, src2);
                     // verify(dest);
                   },

    "num_docs"   => { let idx_name   = &args[2];
                      let num_docs   = with_doc_offsets(idx_name, &|DocOffsetsRead(offs)| offs.len() as u32);
                      let serialized = serde_json::to_string(&NumDocs{num_docs: num_docs}).unwrap();
                      println!("{}", serialized);
                    },

    "query"      => { let query_params = parse_query_params_stdin(&args);
                      with_index_read(&query_params.base_path, &|ir| {
                          let results    = query(&ir, &query_params);
                          let serialized = serde_json::to_string(&results).unwrap();
                          println!("{}", serialized);
                    })},

    "unscored"   => { let query_params = parse_query_params_stdin(&args);
                      with_index_read(&query_params.base_path, &|ir| {
                          let results    = unscored_query(&ir, &query_params);
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
