use serde::Deserialize;

use crate::doc::*;
use crate::normalise::*;
use crate::terms::*;
use crate::types::*;

use std::io::{Read, stdin};

#[derive(Deserialize)]
pub struct Input { pub docs: Vec<InputDoc>
                 }

#[derive(Deserialize)]
pub struct InputDoc { pub url:         String
                    , pub content:     String
                    , pub compression: Option<String>
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

pub struct QueryParams { pub max_results: Option<u32>
                       , pub base_path:   String
                       , pub query_terms: Vec<Term>
                       }

pub fn parse_query_params(args: &Vec<String>) -> QueryParams {

    let mut buffer = String::new();
    stdin().read_to_string(&mut buffer).unwrap();
    let terms = normalise(&buffer);

    let base_path: &String = 
            args.iter()
                .filter(|x| !x.starts_with("-"))
                .skip(2) // Fix this crap
                .next()
                .unwrap();

    QueryParams { max_results: get_max_results(args)
                , base_path:   base_path.to_owned()
                , query_terms: terms
                }
}

fn get_max_results(args: &Vec<String>) -> Option<u32> {
    let mut max_results = None;
    for arg in args {
        if arg.starts_with("-max_results=") {
            let parts: Vec<& str> = arg.split("=").collect();
            max_results = Some(parts[1].parse::<u32>().unwrap());
            break;
        }
    }
    max_results
}