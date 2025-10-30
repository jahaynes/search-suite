use crate::index_reader::*;
use crate::types::*;

use byteorder::{LittleEndian, WriteBytesExt};
use itertools::Itertools;
use memmap::MmapMut;
use rand::prelude::*;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::BufWriter;
use std::io::Write;

pub fn resolve_common_docids(idx_name_a:                 &str,
                             idx_name_b:                 &str,
                             common_docids_path:         &str,
                             common_docids_resolve_path: &str) -> bool {
    with_doc_offsets(idx_name_a, &|offs_a|
    with_docs(       idx_name_a, &|docs_a|
    with_doc_offsets(idx_name_b, &|offs_b|
    with_docs(       idx_name_b, &|docs_b|
        doc_id_collision_resolutions(common_docids_path,
                                     common_docids_resolve_path,
                                     &offs_a,
                                     &docs_a,
                                     &offs_b,
                                     &docs_b)
    ))))
}

fn doc_id_collision_resolutions(common_docids_path:         &str,
                                common_docids_resolve_path: &str,
                                DocOffsetsRead(offs_a):     &DocOffsetsRead,
                                docs_a:                     &DocsRead,
                                DocOffsetsRead(offs_b):     &DocOffsetsRead,
                                docs_b:                     &DocsRead) -> bool {

    let docids_a = offs_a.iter().map(|off_a| read_doc_at(&docs_a, *off_a as usize).doc_id);
    let docids_b = offs_b.iter().map(|off_b| read_doc_at(&docs_b, *off_b as usize).doc_id);

    let common =
        docids_a.merge_by(docids_b, |DocId(x), DocId(y)| x <= y)
                .map(|l| (l, 1))
                .coalesce(|(u1,c1),(u2,c2)| {
                    if u1 == u2 {
                        Ok((u1, c1 + c2))
                    } else {
                        Err(((u1,c1),(u2,c2)))}})
                .filter(|(_u,c)| *c > 1)
                .map(|(u,_c)| u);

    let mut num_dupes = 0;

    let mut common_docids_file_writer =
        BufWriter::new(File::create(&common_docids_path).unwrap());

    // Write out each duplicate docid
    for DocId(dupe) in common {
        println!("Dupe found: {}", dupe);
        num_dupes += 1;
        common_docids_file_writer.write_u32::<LittleEndian>(dupe).unwrap();
    }

    common_docids_file_writer.flush().unwrap();

    if num_dupes == 0 {
        return false;
    }

    let mut find_free_docid =
        FindFreeDocId { offs_a: &DocOffsetsRead(offs_a)
                      , docs_a: &docs_a
                      , offs_b: &DocOffsetsRead(offs_b)
                      , docs_b: &docs_b
                      };

    // Prepare a candidate set of remap targets
    with_u32_mut_vec(common_docids_resolve_path,
                     Some(num_dupes),
                     &mut|buf| {

        // zero initialise
        for i in 0..num_dupes as usize {
            buf[i];
        }

        // WARN unbounded
        let mut contains_dupe = true;
        while contains_dupe {

            // Take a pass, replacing all 0s with candidates
            for i in 0..num_dupes as usize {
                if buf[i] == 0 {
                    let DocId(di) = find_free_docid.next().unwrap();
                    buf[i] = di;
                }
            }

            buf.sort();

            // Detect duplicates
            contains_dupe = false;
            for i in 0..(num_dupes as usize - 1) {
                if buf[i] == buf[i+1] {
                    buf[i+1] = 0;
                    contains_dupe = true;
                }
            }
        }
    });

    true
}

struct FindFreeDocId<'a> {
    offs_a: &'a DocOffsetsRead<'a>,
    docs_a: &'a DocsRead<'a>,
    offs_b: &'a DocOffsetsRead<'a>,
    docs_b: &'a DocsRead<'a>
}

impl Iterator for FindFreeDocId<'_> {
    type Item = DocId;
    fn next(&mut self) -> Option<DocId> {
        let mut candidate;
        // WARN unbounded
        loop {
            candidate = DocId(rand::rng().random());
            if candidate == DocId(0)                           ||
               find_docid(self.offs_a, self.docs_a, candidate) ||
               find_docid(self.offs_b, self.docs_b, candidate) {
                   continue;                   
            } else {
                   break;      
            }
        }
        Some(candidate)
    }
}

pub fn with_u32_mut_vec<A>(file_name:        &str,
                           opt_len_elements: Option<u64>,
                           f:                &mut dyn FnMut(&mut [u32]) -> A) -> A {

    let file = OpenOptions::new().create(true).read(true).write(true).open(file_name).unwrap();

    match opt_len_elements {
        Some(len) => file.set_len(4 * len).unwrap(),
        None      => {}
    }

    let x;
    unsafe {
        let mut mmap = MmapMut::map_mut(&file).expect("failed to map fast terms file");
        let (prefix, data, suffix) = mmap.align_to_mut::<u32>();
        debug_assert!(prefix.is_empty());
        debug_assert!(suffix.is_empty());
        x = f(data);
    };

    x
}

pub fn with_u64_mut_vec<A>(file_name:        &str,
                           opt_len_elements: Option<u64>,
                           f:                &mut dyn FnMut(&mut [u64]) -> A) -> A {

    let file = OpenOptions::new().create(true).read(true).write(true).open(file_name).unwrap();

    match opt_len_elements {
        Some(len) => file.set_len(8 * len).unwrap(),
        None      => {}
    }

    let x;
    unsafe {
        let mut mmap = MmapMut::map_mut(&file).expect("failed to map fast terms file");
        let (prefix, data, suffix) = mmap.align_to_mut::<u64>();
        debug_assert!(prefix.is_empty());
        debug_assert!(suffix.is_empty());
        x = f(data);
    };
    x
}
