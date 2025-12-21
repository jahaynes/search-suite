// Protobuf representation of the JSON input used by the indexer (Input and InputDoc)
#[derive(::prost::Message, Clone, PartialEq)]
pub struct InputDoc {
    #[prost(string, tag = "1")]
    pub url: String,

    #[prost(string, tag = "2")]
    pub content: String,

    // optional compression field (matches Option<String> in the JSON type)
    #[prost(string, optional, tag = "3")]
    pub compression: ::core::option::Option<String>,
}

#[derive(::prost::Message, Clone, PartialEq)]
pub struct Input {
    #[prost(message, repeated, tag = "1")]
    pub docs: ::prost::alloc::vec::Vec<InputDoc>,
}

#[derive(::prost::Message, Clone, PartialEq)]
pub struct IndexResult {
    #[prost(uint32, tag = "1")]
    pub num_docs: u32,

    #[prost(uint32, tag = "2")]
    pub num_terms: u32,

    #[prost(uint64, tag = "3")]
    pub ms_taken: u64,
}



cna't be 0
try optional
https://google.github.io/proto-lens/