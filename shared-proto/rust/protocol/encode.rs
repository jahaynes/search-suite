pub trait Cbor {
    fn cbor(&self) -> Result<Vec<u8>, serde_cbor::Error>;
}

pub trait UnCbor: Sized {
    fn uncbor(bytes: &[u8]) -> Result<Self, serde_cbor::Error>;
}
