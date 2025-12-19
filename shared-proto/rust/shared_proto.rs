
#[derive(::prost::Message, Clone)]
pub struct NewEvent {

    #[prost(bytes, tag = "1")]
    pub some_bytes: Vec<u8>,

    #[prost(string, tag = "2")]
    pub some_text: String,
    
    #[prost(int32, tag = "3")]
    pub some_num: i32,
    
    #[prost(string, repeated, tag = "4")]
    pub metadata_keys: Vec<String>,

    #[prost(string, repeated, tag = "5")]
    pub metadata_values: Vec<String>,
}

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

// Protobuf representation of indexer reply (num_docs, num_terms, ms_taken)
#[derive(::prost::Message, Clone, PartialEq)]
pub struct IndexResult {
    #[prost(uint32, tag = "1")]
    pub num_docs: u32,

    #[prost(uint32, tag = "2")]
    pub num_terms: u32,

    #[prost(uint64, tag = "3")]
    pub ms_taken: u64,
}

pub fn encoded_msg() -> Vec<u8> {
    let msg = NewEvent {
        some_bytes: b"foo".to_vec(),
        some_text: "bar".to_string(),
        some_num: 234,
        metadata_keys: vec!["key1".to_string(), "key2".to_string()],
        metadata_values: vec!["value1".to_string(), "value2".to_string()],
    };

    use prost::Message;
    let mut buf = Vec::new();
    msg.encode(&mut buf).expect("Failed to encode message");
    buf
}

