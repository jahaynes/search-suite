use std::collections::HashSet;
use std::collections::HashMap;
use std::env;
use std::path::*;
use uuid::Uuid;

use doc::*;
use index::*;
use index_reader::*;
use input::*;
use merge::*;
use query::*;
use types::*;
use terms::*;
use verify::*;

fn fresh_dir() -> String {
    let mut dir = env::temp_dir();
    let uuid = format!("{}", Uuid::new_v4());
    dir.push(Path::new(&uuid));
    String::from(dir.to_str().unwrap())
}

fn create_index(url_term_doc_ids: &[(&str, &str, u32)]) -> String {
    
    let docs: Vec<Doc> = 
        url_term_doc_ids.iter()
                        .map(|(url, term, doc_id)| {
                            let mut term_freqs = HashMap::new();
                            term_freqs.insert(Term(String::from(*term)), TermFreq(1));
                            Doc { url:              Url(String::from(*url))
                                , doc_id:           DocId(*doc_id)
                                , term_frequencies: term_freqs
                                , doc_len:          1
                                }})
                        .collect();

    let idx_dir = fresh_dir();
    index(&idx_dir, docs);
    idx_dir
}

fn create_singleton_index(url:    &str,
                          term:   &str,
                          doc_id: u32) -> String {
    create_index(&[(url, term, doc_id)])
}

fn single_term_query(idx_dir:      &str,
                     term:         &str) -> HashSet<Url> {

    let query_params =
        QueryParams { max_results: None
                    , base_path:   idx_dir.to_string()
                    , query_terms: vec![Term(String::from(term))]
                    };

    with_index_read(&query_params.base_path, &|ir| {
        let QueryResult{num_results: _, results} = query(&ir, &query_params);    
        results.iter()
               .map(|sc| Url(sc.uri.to_owned()))
               .collect()
    })
}

#[test]
pub fn singleton_test() {

    let idx_dir_1 = create_singleton_index("http://one",
                                           "one",
                                           1);

    verify(&idx_dir_1);

    let urls = single_term_query(&idx_dir_1, "one");
    assert!(urls.len() == 1);
    assert!(urls.contains(&Url("http://one".to_string())));
}

#[test]
pub fn simple_merge_test() {

    let idx_dir_1 = create_singleton_index("http://one",
                                           "one",
                                           1);

    verify(&idx_dir_1);

    let idx_dir_2 = create_singleton_index("http://two",
                                           "two",
                                           2);

    verify(&idx_dir_2);

    let idx_dir_1_2 = fresh_dir();

    merge::merge(&idx_dir_1_2,
                 &idx_dir_1,
                 &idx_dir_2);

    verify(&idx_dir_1_2);

    let urls = single_term_query(&idx_dir_1_2, "one");
    assert!(urls.len() == 1);
    assert!(urls.contains(&Url("http://one".to_string())));

    let urls = single_term_query(&idx_dir_1_2, "two");
    assert!(urls.len() == 1);
    assert!(urls.contains(&Url("http://two".to_string())));
}

#[test]
pub fn self_merge_test() {

    let idx_dir_1 = create_singleton_index("http://one",
                                           "one",
                                           1);

    verify(&idx_dir_1);

    let idx_dir_1_1 = fresh_dir();

    merge::merge(&idx_dir_1_1,
                 &idx_dir_1,
                 &idx_dir_1);

    let urls = single_term_query(&idx_dir_1_1, "one");
    assert!(urls.len() == 1);
    assert!(urls.contains(&Url("http://one".to_string())));
}

#[test]
pub fn dualing_docid_merge() {

    let idx_dir_1 = create_singleton_index("http://one",
                                           "one",
                                           1);

    let idx_dir_2 = create_singleton_index("http://two",
                                           "two",
                                           1);

    let idx_dir_1_2 = fresh_dir();

    merge::merge(&idx_dir_1_2,
                 &idx_dir_1,
                 &idx_dir_2);

    verify(&idx_dir_1_2);

    let urls = single_term_query(&idx_dir_1_2, "one");
    assert!(urls.len() == 1);
    assert!(urls.contains(&Url("http://one".to_string())));

    let urls = single_term_query(&idx_dir_1_2, "two");
    assert!(urls.len() == 1);
    assert!(urls.contains(&Url("http://two".to_string())));
}

#[test]
pub fn missing_keywords() {

    let idx_dir_1 = create_index( &[ ("http://one-a",   "one",    40)
                                   , ("http://one-b",   "one",    41)
                                   , ("http://two-a",   "two",    42)
                                   , ("http://two-b",   "two",    43)
                                   , ("http://large-a", "large",  44)
                                   , ("http://large-b", "large",  45)
                                   , ("http://three-a", "three",  46)
                                   , ("http://three-b", "three",  47)
                                   , ("http://apple-a", "apple",  48)
                                   , ("http://apple-b", "apple",  49)
                                   ]);

    verify(&idx_dir_1);

    let idx_dir_2 = create_index( &[ ("http://four-a",   "four",   30)  // Included
                                   , ("http://four-b",   "four",   31)
                                   , ("http://five-a",   "five",   32)  // Included
                                   , ("http://five-b",   "five",   33)
                                   , ("http://large-a",  "large",  34)  // should be removed
                                   , ("http://large-b",  "large",  35)  // should be removed
                                   , ("http://six-a",    "six",    36)  // Included
                                   , ("http://six-b",    "six",    37)
                                   , ("http://banana-a", "banana", 38)  // Included
                                   , ("http://banana-b", "banana", 39)
                                   ]);

    verify(&idx_dir_2);

    let idx_dir_1_2 = fresh_dir();

    merge::merge(&idx_dir_1_2,
                 &idx_dir_1,
                 &idx_dir_2);

    verify(&idx_dir_1_2);

    for term in &[  "one",   "two", "three"
                 , "four",  "five", "large"
                 ,  "six", "apple", "banana" ] {

    let urls = single_term_query(&idx_dir_1_2, term);
        println!("{:?}\n{:?}\n", term, urls);
    }
}

#[test]
pub fn dualing_docids_correctly_ordered() {

    let idx_dir_1 = create_index( &[ ("http://one-a",   "one",    1)
                                   , ("http://one-b",   "one",    2)
                                   , ("http://two-a",   "two",    3)
                                   , ("http://two-b",   "two",    4)
                                   , ("http://large-a", "large",  536870912)  // x `shiftL` 29 
                                   , ("http://large-b", "large",  1073741824) // x `shiftL` 30
                                   , ("http://three-a", "three",  5)
                                   , ("http://three-b", "three",  6)
                                   , ("http://apple-a", "apple", 31)
                                   , ("http://apple-b", "apple", 32)
                                   ]);

    verify(&idx_dir_1);

    let idx_dir_2 = create_index( &[ ("http://four-a",   "four",    1)
                                   , ("http://four-b",   "four",    2)
                                   , ("http://five-a",   "five",    3)
                                   , ("http://five-b",   "five",    4)
                                   , ("http://large-a",  "large",   536870912)  // x `shiftL` 29
                                   , ("http://large-b",  "large",   1073741824) // x `shiftL` 30
                                   , ("http://six-a",    "six",     5)
                                   , ("http://six-b",    "six",     6)
                                   , ("http://banana-a", "banana", 33)
                                   , ("http://banana-b", "banana", 34)
                                   ]);

    verify(&idx_dir_2);

    let idx_dir_1_2 = fresh_dir();

    merge::merge(&idx_dir_1_2,
                 &idx_dir_1,
                 &idx_dir_2);

    verify(&idx_dir_1_2);

    for term in &[  "one",   "two", "three"
                 , "four",  "five", "large"
                 ,  "six", "apple", "banana" ] {

        let urls = single_term_query(&idx_dir_1_2, term);
        assert!(urls.len() == 2, "MISSING RESULTS for {}", term);
        assert!(urls.contains(&Url(format!("http://{}-a",term))));
        assert!(urls.contains(&Url(format!("http://{}-b",term))));
    }
}