#[cfg(test)]
use indexer_qp2::normalise::normalise;
#[cfg(test)]
use indexer_qp2::terms::Term;

#[cfg(test)]
#[test]
fn test_empty_string() {
    assert_eq!(normalise(""), Vec::<Term>::new());
}

#[cfg(test)]
#[test]
fn test_only_non_alphanumeric() {
    let result = normalise("!!!@@@###");
    assert_eq!(result, Vec::<Term>::new());
}

#[cfg(test)]
#[test]
fn test_single_word() {
    let result = normalise("Hello");
    assert_eq!(result, vec![Term("hello".to_string())]);
}

#[cfg(test)]
#[test]
fn test_multiple_words() {
    let result = normalise("Hello World");
    assert_eq!(result, vec![Term("hello".to_string()), Term("world".to_string())]);
}

#[cfg(test)]
#[test]
fn test_mixed_alphanumeric() {
    let result = normalise("Hello123World456");
    assert_eq!(result, vec![Term("hello123world456".to_string())]);
}

#[cfg(test)]
#[test]
fn test_mixed_with_spaces() {
    let result = normalise("Hello 123 World 456");
    assert_eq!(result, vec![Term("hello".to_string()), Term("123".to_string()), Term("world".to_string()), Term("456".to_string())]);
}

#[cfg(test)]
#[test]
fn test_unicode() {
    let result = normalise("café naïve");
    assert_eq!(result, vec![Term("café".to_string()), Term("naïve".to_string())]);
}

#[cfg(test)]
#[test]
fn test_mixed_case() {
    let result = normalise("HeLLo WoRLD");
    assert_eq!(result, vec![Term("hello".to_string()), Term("world".to_string())]);
}
