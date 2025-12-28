use super::encode::{Cbor, UnCbor};

use serde::Deserialize;

use serde_cbor::{Error, to_vec};

#[derive(Debug, Deserialize)]
pub struct InputDoc {
    pub url: String,
    pub content: String,
    pub compression: String
}

impl UnCbor for InputDoc {
    fn uncbor(bytes: &[u8]) -> Result<Self, Error> {
        serde_cbor::from_slice(bytes)
    }
}

impl UnCbor for Vec<InputDoc> {
    fn uncbor(bytes: &[u8]) -> Result<Self, Error> {
        serde_cbor::from_slice(bytes)
    }
}

#[derive(Debug, Deserialize)]
pub struct Input {
    pub docs: Vec<InputDoc>
}

#[derive(Debug)]
pub struct IndexReply {
    pub num_docs: u32,
    pub num_terms: u32,
    pub ms_taken: u64
}

impl Cbor for IndexReply {
    fn cbor(&self) -> Result<Vec<u8>, Error> {
        to_vec(&( self.num_docs
                , self.num_terms
                , self.ms_taken
                ))
    }
}
