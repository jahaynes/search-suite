use num::integer::{div_ceil, div_rem};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::cmp;

use index_reader::*;
use types::*;

pub fn write_empty_deletions_file(doc_offsets_length: usize,
                                  deleted_docs_file: File) {
    let num_bytes = div_ceil(doc_offsets_length, 8);
    let mut writer = BufWriter::new(deleted_docs_file);
    let mut buffer = [0; 1024];
    let mut remaining_bytes = num_bytes;
    while remaining_bytes > 0 {
        let to_write = cmp::min(remaining_bytes, buffer.len());
        let buffer = &mut buffer[..to_write];
        writer.write(buffer).unwrap();
        remaining_bytes -= to_write;
    }
}

// TODO Batch it?
// maybe use docoffset instead
pub fn is_deleted(idx_dir: &str,
                  url: &Url) -> String {

  let deletions_file = &format!("{}/{}", idx_dir, "docDeletions");

  let status =
    if_doc_exists(idx_dir, url, &|doc_off_num, _off, _doc_entry|
      with_vec(deletions_file, &|bytes:&[u8]| is_deleted2(bytes, doc_off_num)));

  match status { None        => "MISSING"
               , Some(false) => "PRESENT"
               , Some(true)  => "DELETED"
               }.to_string()
}

pub fn is_deleted2(deletions:&[u8],
                   doc_off_num: u32) -> bool {
  let (byte, bit) = div_rem(doc_off_num, 8);
  let mask : u8 = 1 << (bit as u8);
  deletions[byte as usize] & mask != 0
}

// needs linear search through docs
// set a single bit in docDeletions corresponding to the doc's offset
pub fn delete_document(idx_dir: &str,
                       url: &Url) {

    if_doc_exists(idx_dir,
                  url,
                  &|doc_off_num, _off, _doc_entry| {
                      let deletions_file = &format!("{}/{}", idx_dir, "docDeletions");
                      with_mut_vec(deletions_file, &|bytes:&mut[u8]| {
                        let (byte, bit) = div_rem(doc_off_num, 8);
                        let mask : u8 = 1 << (bit as u8);
                        bytes[byte as usize] |= mask;
                      });
                    });
}
