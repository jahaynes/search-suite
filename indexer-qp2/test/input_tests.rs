#[cfg(test)]
use indexer_qp2::input::*;
#[cfg(test)]
use indexer_qp2::terms::Term;

fn default_query_params(base_path: String) -> QueryParams {
    QueryParams {
        base_path: base_path,
        max_results: None,
        scoring: None,
        mode: None,
        query_terms: vec![],
    }
}

#[cfg(test)]
#[test]
fn test_parse_query_params_defaults() {

    let base_path = "/index/path";

    let expected = default_query_params(base_path.to_string());

    let result = parse_query_params( &vec![ "--base_path".to_string()
                                          , base_path.to_string() ]
                                   , "".to_string() );

    assert_eq!(expected, result);
}

#[cfg(test)]
#[test]
fn test_parse_query_params_options() {

    let base_path = "/index/path";

    let result = parse_query_params( &vec![ "--base_path".to_string()
                                          , base_path.to_string()
                                          , "--max_results".to_string()
                                          , "3".to_string()
                                          , "--scoring".to_string()
                                          , "bm25".to_string()
                                          , "--mode".to_string()
                                          , "regex".to_string() ]
                                   , "".to_string() );

    let mut expected = default_query_params(base_path.to_string());
    expected.max_results = Some(3);
    expected.scoring     = Some(Scoring::BM25);
    expected.mode        = Some(Mode::Regex);
    expected.query_terms = vec![(Term("".to_string()))];
    assert_eq!(expected, result);
}
