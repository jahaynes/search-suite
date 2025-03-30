use terms::Term;

use itertools::Itertools;

pub fn normalise(s: &str) -> Vec<Term> {
    s.chars()
     .chunk_by( |c| c.is_alphanumeric())
     .into_iter()
     .filter( |(a,_)| *a)
     .map( |(_,b)| b)
     .map( |gr| gr.collect::<String>().to_lowercase() )
     .map( |t| Term(t) )
     .collect::<Vec<Term>>()
}
