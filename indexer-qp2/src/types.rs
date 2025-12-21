use serde::Serialize;
use std::ops::AddAssign;

#[derive(Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Url(pub String);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Serialize)]
pub struct DocId(pub u32);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TermFreq(pub u32);

impl AddAssign for &mut TermFreq {
    fn add_assign(&mut self, other: Self) {
        let TermFreq(a) = self;
        let TermFreq(b) = other;
        **self = TermFreq(*a + *b);
    }
}

pub struct Escaped(pub Vec<u8>);

pub struct Postings(pub Vec<u32>);
pub struct PostingsRead<'a>(pub &'a [u32]);

pub struct Docs(pub Escaped);
pub struct DocsRead<'a>(pub &'a [u8]);

pub struct DocOffsets(pub Vec<u64>);
pub struct DocOffsetsRead<'a>(pub &'a[u64]);

pub struct DocDeletionsRead<'a>(pub &'a[u8]);

#[derive(Serialize)]
pub struct NumDocs {
    pub num_docs: u32
}
