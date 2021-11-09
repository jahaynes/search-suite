pub trait WriteToBuf {
    fn write_to_buf(&self, _: &mut Vec<u8>) -> usize;
}