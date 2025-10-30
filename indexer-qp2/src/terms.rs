use crate::bytes::*;
use crate::types::Escaped;
use crate::write_to_buf::WriteToBuf;

#[derive(Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Term(pub String);

pub struct Terms(pub Escaped);
pub struct TermsRead<'a>(pub &'a [u8]);

#[derive(Debug)]
pub struct TermEntry { pub term:     Term
                     , pub offset:   u64
                     , pub doc_freq: u32
                     }

pub struct TermOffsets(pub Vec<u64>);
pub struct TermOffsetsRead<'a>(pub &'a [u64]);

impl WriteToBuf for TermEntry {

    fn write_to_buf(&self, buf: &mut Vec<u8>) -> usize {

        let mut written = 0;

        // term
        let Term(term) = &self.term;
        written += write_esc(buf, term.as_bytes());

        // "\0"
        buf.push(b'\0');
        written += 1;

        // off u64
        written += write_esc_u64(buf, self.offset);

        // df u32
        written += write_esc_u32(buf, self.doc_freq);

        // newline (not escaped)
        buf.push(b'\n');
        written += 1;
        
        written
    }
}
