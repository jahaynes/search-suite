use crate::doc::*;
use crate::normalise::*;
use crate::terms::*;
use crate::types::*;

use shared_proto::protocol::encode::UnCbor;
use shared_proto::protocol::types::InputDoc;

use std::io::{Read, stdin};

#[derive(Debug)]
pub enum Scoring {
    BM25
}

#[derive(Debug)]
pub enum Mode {
    Regex
}

pub fn docs_from_protobuf_stdin() -> Vec<Doc> {
    let mut buf: Vec<u8> = Vec::new();
    stdin().read_to_end(&mut buf).unwrap();
    let decoded: Vec<InputDoc> = Vec::uncbor(&buf).expect("cborg issue");
    // TODO: reconsider just passing through the actual type 
    let mut docs = Vec::<Doc>::new();
    for input_doc in decoded {
        docs.push(mk_doc(Url(input_doc.url), &input_doc.content));
    }
    docs
}

#[derive(Debug)]
pub struct QueryParams { pub base_path:   String            //  --base_path=
                       , pub max_results: Option<u32>       //  --max_results=
                       , pub scoring:     Option<Scoring>   //  --scoring=
                       , pub mode:        Option<Mode>      //  --mode=regex
                       , pub query_terms: Vec<Term>         //  stdin
                       }

pub fn parse_query_params(args: &Vec<String>) -> QueryParams {

    enum ArgMode {
        BasePath,
        MaxResults,
        Scoring,
        Mode
    }

    // Process command line arguments

    let mut base_path   : Option<String>  = None;
    let mut max_results : Option<u32>     = None;
    let mut scoring     : Option<Scoring> = None;
    let mut mode        : Option<Mode>    = None;

    let mut arg_mode : Option<ArgMode> = None;
    for a in args {
        if a.starts_with("--") {
            match a.as_str() {
                "--base_path"   => arg_mode = Some(ArgMode::BasePath),
                "--max_results" => arg_mode = Some(ArgMode::MaxResults),
                "--mode"        => arg_mode = Some(ArgMode::Mode),
                "--scoring"     => arg_mode = Some(ArgMode::Scoring),
                _               => panic!("unknown flag: {}", a),
            }
        } else {
            match arg_mode {

                Some(ArgMode::BasePath) =>
                    base_path = Some(a.clone()),

                Some(ArgMode::MaxResults) =>
                    max_results = Some(a.parse::<u32>().unwrap()),

                Some(ArgMode::Scoring) =>
                    match a.as_str() {
                        "bm25" => scoring = Some(Scoring::BM25),
                        _      => panic!("unknown scoring: {}", a),
                    },

                Some(ArgMode::Mode) =>
                    match a.as_str() {
                        "regex" => mode = Some(Mode::Regex),
                        _       => panic!("unknown mode: {}", a),
                    },

                None => {}
            }
        }
    }

    // Extract query terms from STDIN

    let mut buffer = String::new();
    stdin().read_to_string(&mut buffer).unwrap();

    let terms =
        match mode {
            // In the case of Regex, let the raw string through
            Some(Mode::Regex) => vec!(Term(buffer)),
            None              => normalise(&buffer)
        };

    return QueryParams { base_path   : base_path.unwrap_or_else(|| panic!("unspecified: --base-path"))
                       , max_results : max_results
                       , scoring     : scoring
                       , mode        : mode
                       , query_terms : terms
                       }
}
