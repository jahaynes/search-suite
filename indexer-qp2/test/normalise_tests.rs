#[cfg(test)]
use indexer_qp2::normalise::normalise;
#[cfg(test)]
use indexer_qp2::terms::Term;

#[cfg(test)]
#[test]
fn test_empty_string() {
    assert_eq!( normalise("")
              , Vec::<Term>::new());
}

#[cfg(test)]
#[test]
fn test_only_non_alphanumeric() {
    assert_eq!( normalise("!!!@@@###")
              , Vec::<Term>::new());
}

#[cfg(test)]
#[test]
fn test_single_word() {
    assert_eq!( normalise("One")
              , vec![ Term("one".to_string())]);
}

#[cfg(test)]
#[test]
fn test_multiple_words() {

    assert_eq!( normalise("Hello World")
              , vec![ Term("hello".to_string())
                    , Term("world".to_string())]);
}

#[cfg(test)]
#[test]
fn test_mixed_alphanumeric() {

    assert_eq!( normalise("Hello123World456")
              , vec![ Term("hello".to_string())
                    , Term("123".to_string())
                    , Term("world".to_string())
                    , Term("456".to_string())]);
}

#[cfg(test)]
#[test]
fn test_mixed_with_spaces() {

    assert_eq!( normalise("Hello 123 World 456")
              , vec![ Term("hello".to_string())
                    , Term("123".to_string())
                    , Term("world".to_string())
                    , Term("456".to_string())
                    ]);
}

#[cfg(test)]
#[test]
fn test_unicode() {

    assert_eq!( normalise("café naïve")
              , vec![ Term("café".to_string())
                    , Term("naïve".to_string())]);
}

#[cfg(test)]
#[test]
fn test_deagglutinate() {

    assert_eq!( normalise("helloWorld")
              , vec![ Term("hello".to_string())
                    , Term("helloworld".to_string())
                    , Term("world".to_string())]);

    assert_eq!( normalise("HelloWorld")
              , vec![ Term("hello".to_string())
                    , Term("helloworld".to_string())
                    , Term("world".to_string())]);
}
