extern crate byteorder;
extern crate flate2;
extern crate intmap;
extern crate itertools;
extern crate memmap;
extern crate rand;
extern crate serde;
extern crate uuid;

mod bytes;
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
mod terms;
mod types;
mod verify;
mod write_to_buf;

#[cfg(test)]
mod byte_tests;
#[cfg(test)]
mod system_tests;

use dump::*;
use index::*;
use index_reader::*;
use input::*;
use query::*;
use types::*;
use verify::*;

use std::env;

fn main() {

  let args: Vec<String> = env::args().collect();

  match args[1].as_str() {

    "dump"       => dump(&args[2]),

    "index_json" => { let input_docs = docs_from_stdin();
                      index(&args[2], input_docs);
                      // verify(&args[2]);
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

    "verify"     => verify(&args[2]),

    _            => panic!("usage")
  }
}
