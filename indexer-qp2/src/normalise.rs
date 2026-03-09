use crate::terms::Term;

use itertools::Itertools;
use slice_group_by::StrGroupBy;

#[derive(PartialEq, Eq)]
enum CharType {
    Alpha,
    Num,
    Other
}

pub fn normalise(s: &str) -> Vec<Term> {
    s.chars()
     .chunk_by(char_type)
     .into_iter()
     .flat_map(|(t, g)| match t {
         CharType::Alpha => deagglutinate_alpha(g.collect::<String>()),
         CharType::Num   => vec![Term(g.collect::<String>())],
         CharType::Other => vec![]
     })
     .collect()
}

fn char_type(c: &char) -> CharType {
    if c.is_alphabetic() {
        CharType::Alpha
    } else if c.is_numeric() {
        CharType::Num
    } else {
        CharType::Other
    }
}

fn deagglutinate_alpha(s: String) -> Vec<Term> {
    s.linear_group_by(|a, b| !(a.is_lowercase() && b.is_uppercase()))
     .enumerate()
     .map(|(i, t)| if i == 1 { vec![s.to_lowercase(), t.to_lowercase()] } else { vec![t.to_lowercase()]} )
     .flatten()
     .map(Term)
     .collect()
}
