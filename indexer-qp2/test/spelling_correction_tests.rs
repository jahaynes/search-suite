#[cfg(test)]
use crate::util::fresh_tmp_dir;
#[cfg(test)]
use indexer_qp2::spelling_correction;
#[cfg(test)]
use indexer_qp2::terms::*;
#[cfg(test)]
use indexer_qp2::types::Escaped;

// Helper function to create a simple terms structure for testing
fn create_test_terms(terms: &[&str]) -> (Terms, TermOffsets) {
    let mut term_bytes = Vec::new();
    let mut offsets = Vec::new();
    let mut current_offset = 0u64;

    for term in terms {
        offsets.push(current_offset);
        let term_bytes_term = term.as_bytes();
        term_bytes.extend_from_slice(term_bytes_term);
        term_bytes.push(b'\0'); // null terminator
        current_offset += term_bytes_term.len() as u64 + 1;
    }

    (Terms(Escaped(term_bytes)), TermOffsets(offsets))
}

#[cfg(test)]
#[test]
fn test_query_bk_empty_tree() {
    let (terms, term_offsets) = create_test_terms(&["apple"]);
    
    let fp = fresh_tmp_dir("test_query_bk_empty_");
    let path = fp.as_ref();

    // Create spelling index
    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);

    // Query with a term that doesn't exist
    let query_term = "banana".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 1);

    // Should return empty or only exact matches
    assert!(result.contains_key(&0) || result.is_empty());
}

#[cfg(test)]
#[test]
fn test_query_bk_exact_match() {
    let (terms, term_offsets) = create_test_terms(&["apple", "banana", "cherry"]);
    
    let fp = fresh_tmp_dir("test_query_bk_exact_");
    let path = fp.as_ref();

    // Create spelling index
    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);

    // Query with exact match
    let query_term = "apple".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 0);

    assert!(result.contains_key(&0));
    let matches = result.get(&0).unwrap();
    assert!(matches.contains(&"apple".to_string()));
}

#[cfg(test)]
#[test]
fn test_query_bk_near_match() {
    let (terms, term_offsets) = create_test_terms(&["apple", "apricot", "banana"]);
    
    let fp = fresh_tmp_dir("test_query_bk_near_");
    let path = fp.as_ref();

    // Create spelling index
    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);

    // Query with near match (apple vs appke - 1 edit distance)
    let query_term = "appke".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 1);

    // Should find "apple" with distance 1
    assert!(result.contains_key(&1));
    let matches = result.get(&1).unwrap();
    assert!(matches.contains(&"apple".to_string()));
}

#[cfg(test)]
#[test]
fn test_query_bk_multiple_distances() {
    let (terms, term_offsets) = create_test_terms(&["test", "testing", "tested", "tester"]);
    
    let fp = fresh_tmp_dir("test_query_bk_multi_");
    let path = fp.as_ref();

    // Create spelling index
    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);

    // Query with a term that could match at different distances
    let query_term = "testng".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 2);

    // Should find matches at various distances
    assert!(!result.is_empty());
    
    // Verify we can iterate over all distance buckets
    for (dist, matches) in result.iter() {
        assert!(*dist >= 0);
        assert!(!matches.is_empty());
    }
}

#[cfg(test)]
#[test]
fn test_mk_spell_correction_basic() {
    let (terms, term_offsets) = create_test_terms(&["one", "two", "three", "four", "five"]);
    
    let fp = fresh_tmp_dir("test_mk_basic_");
    let path = fp.as_ref();

    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    
    // Should complete without error
    let duration = spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);
    
    // Duration should be reasonable (less than 10 seconds for this small test)
    assert!(duration.as_secs() < 10);
    
    // Verify file was created
    let spelling_file = format!("{}/spelling", path);
    assert!(std::path::Path::new(&spelling_file).exists());
}

#[cfg(test)]
#[test]
fn test_mk_spell_correction_single_term() {
    let (terms, term_offsets) = create_test_terms(&["solo"]);
    
    let fp = fresh_tmp_dir("test_mk_single_");
    let path = fp.as_ref();

    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    
    let duration = spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);
    
    assert!(duration.as_secs() < 10);
    
    // Query should find the single term
    let query_term = "solo".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 0);
    
    assert!(result.contains_key(&0));
    let matches = result.get(&0).unwrap();
    assert!(matches.contains(&"solo".to_string()));
}

#[cfg(test)]
#[test]
fn test_mk_spell_correction_empty_input() {
    // Test with empty terms - this should handle gracefully
    let (terms, term_offsets) = create_test_terms(&[]);
    
    let fp = fresh_tmp_dir("test_mk_empty_");
    let path = fp.as_ref();

    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    
    // Should not panic
    let duration = spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);
    
    // Duration should be very quick for empty input
    assert!(duration.as_secs() < 10);
}

#[cfg(test)]
#[test]
fn test_query_bk_with_max_distance() {
    let (terms, term_offsets) = create_test_terms(&["hello", "world", "test"]);
    
    let fp = fresh_tmp_dir("test_query_max_dist_");
    let path = fp.as_ref();

    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);

    // Query with a large max distance to catch more matches
    let query_term = "helo".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 3);

    // Should find "hello" with distance 1
    assert!(result.contains_key(&1));
    let matches = result.get(&1).unwrap();
    assert!(matches.contains(&"hello".to_string()));
}

#[cfg(test)]
#[test]
fn test_query_bk_no_matches() {
    let (terms, term_offsets) = create_test_terms(&["apple", "banana", "cherry"]);
    
    let fp = fresh_tmp_dir("test_query_no_match_");
    let path = fp.as_ref();

    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);

    // Query with a term that has no close matches
    let query_term = "xyzabc".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 1);

    // Should not find any close matches
    // May or may not have exact match (distance 0)
    // But should not have distance 1 matches
    if result.contains_key(&1) {
        let matches = result.get(&1).unwrap();
        assert!(matches.is_empty(), "Should have no distance-1 matches");
    }
}

#[cfg(test)]
#[test]
fn test_query_bk_unicode_terms() {
    let (terms, term_offsets) = create_test_terms(&["café", "naïve", "résumé"]);
    
    let fp = fresh_tmp_dir("test_query_unicode_");
    let path = fp.as_ref();

    let term_offsets_read = TermOffsetsRead(term_offsets.0.as_slice());
    let terms_read = TermsRead(terms.0 .0.as_slice());
    spelling_correction::mk_spell_correction(path, &term_offsets_read, &terms_read);

    // Query with a term that has a typo
    let query_term = "cafe".to_string();
    let result = spelling_correction::query_bk(path, &query_term, 1);

    // Should find "café" with distance 1
    assert!(result.contains_key(&1));
    let matches = result.get(&1).unwrap();
    assert!(matches.contains(&"café".to_string()));
}