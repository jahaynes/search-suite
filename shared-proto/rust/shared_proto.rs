use serde::Deserialize;
use serde::Serialize;

use serde_cbor::{Error, from_slice, to_vec};

/*
#[derive(Debug, Deserialize)]
pub struct InputDoc {
    pub compression: String,
    pub content: String,
    pub url: String
}

#[derive(Debug, Deserialize)]
pub struct Input {
    pub docs: Vec<InputDoc>
}
*/

#[derive(Debug)]
pub struct IndexReply {
    pub num_docs: u32,
    pub num_terms: u32,
    pub ms_taken: u64
}

pub trait Cbor {
    fn cbor(&self) -> Result<Vec<u8>, Error>;
}

impl Cbor for IndexReply {
    fn cbor(&self) -> Result<Vec<u8>, Error> {
        to_vec(&( self.num_docs
                , self.num_terms
                , self.ms_taken
                ))
    }
}
