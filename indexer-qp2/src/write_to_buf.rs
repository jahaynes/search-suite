pub trait WriteToBuf {
    fn write_to_buf(&self, &mut Vec<u8>) -> usize;
}