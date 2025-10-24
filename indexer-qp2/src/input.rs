use doc::*;
use normalise::*;
use terms::*;
use types::*;

use serde::Deserialize;
use std::io::{Read, stdin};

#[derive(Deserialize)]
pub struct Input { pub docs: Vec<InputDoc>
                 }

#[derive(Deserialize)]
pub struct InputDoc { pub url:         String
                    , pub content:     String
                    , pub compression: Option<String>
                    }

#[derive(Debug)]
pub enum Scoring {
    BM25
}

pub fn docs_from_stdin() -> Vec<Doc> {
    let mut buffer = String::new();
    stdin().read_to_string(&mut buffer).unwrap();
    let deserialized: Input = serde_json::from_str(&buffer).unwrap();
    let mut docs = Vec::<Doc>::new();
    for input_doc in deserialized.docs {
        docs.push(mk_doc(Url(input_doc.url), &input_doc.content));
    }
    docs
}

// Read a single doc from stdin to bypass the json encoding/decoding
pub fn doc_from_stdin() -> Doc {
    let url = stdin().lines().next().unwrap().unwrap();
    let mut buffer = String::new();
    stdin().read_to_string(&mut buffer).unwrap();
    mk_doc(Url(url), &buffer)
}

#[derive(Debug)]
pub struct QueryParams { pub base_path:   String            //  --base_path=
                       , pub max_results: Option<u32>       //  --max_results=
                       , pub scoring:     Option<Scoring>   //  --scoring=

                       , pub query_terms: Vec<Term>         //  stdin
                       }

pub fn parse_query_params(args: &Vec<String>) -> QueryParams {

    enum ArgMode {
        BasePath,
        MaxResults,
        Scoring
    }

    // Process command line arguments

    let mut base_path   : Option<String>  = None;
    let mut max_results : Option<u32>     = None;
    let mut scoring     : Option<Scoring> = None;

    let mut arg_mode : Option<ArgMode> = None;
    for a in args {
        if a.starts_with("--") {
            match a.as_str() {
                "--base_path"   => arg_mode = Some(ArgMode::BasePath),
                "--max_results" => arg_mode = Some(ArgMode::MaxResults),
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

                None => {}
            }
        }
    }

    // Extract query terms from STDIN

    let mut buffer = String::new();
    stdin().read_to_string(&mut buffer).unwrap();
    let terms = normalise(&buffer);

    return QueryParams { base_path   : base_path.unwrap_or_else(|| panic!("unspecified: --base-path"))
                       , max_results : max_results
                       , scoring     : scoring
                       , query_terms : terms
                       }
}
