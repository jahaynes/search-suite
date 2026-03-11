#[cfg(test)]
use indexer_qp2::ranking::*;

#[cfg(test)]
#[test]
fn test_scored_ord() {
    // Test ordering of Scored structs
    let scored1 = Scored {
        uri: "http://example.com/1".to_string(),
        score: 1.5,
        term_count: 1,
    };
    let scored2 = Scored {
        uri: "http://example.com/2".to_string(),
        score: 2.5,
        term_count: 1,
    };
    let scored3 = Scored {
        uri: "http://example.com/3".to_string(),
        score: 1.5,
        term_count: 2,
    };

    // Higher score should come first
    assert!(scored2 > scored1);
    assert!(scored2 > scored3);

    // Same score - term_count doesn't affect ordering in current implementation
    assert_eq!(scored1, scored3);
}

#[cfg(test)]
#[test]
fn test_scored_eq() {
    // Test equality of Scored structs
    let scored1 = Scored {
        uri: "http://example.com/1".to_string(),
        score: 1.5,
        term_count: 1,
    };
    let scored2 = Scored {
        uri: "http://example.com/2".to_string(),
        score: 1.5,
        term_count: 2,
    };

    // Equal scores should be equal (term_count doesn't affect equality)
    assert_eq!(scored1, scored2);
}

#[cfg(test)]
#[test]
fn test_scored_ord_same_score() {
    // Test ordering when scores are the same
    let scored1 = Scored {
        uri: "http://example.com/1".to_string(),
        score: 1.5,
        term_count: 1,
    };
    let scored2 = Scored {
        uri: "http://example.com/2".to_string(),
        score: 1.5,
        term_count: 2,
    };

    // Same score means equal (term_count doesn't affect ordering)
    assert_eq!(scored1, scored2);
}