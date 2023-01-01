
use bytes::*;
use doc::*;
use terms::*;
use types::*;

use memmap::{Mmap, MmapMut};
use std::fs::{File, OpenOptions};
use std::str::from_utf8;
use std::io::BufReader;
use std::io::prelude::*;

pub struct IndexRead<'a>{ pub total_doc_len: u64 
                        , pub term_offsets:  &'a TermOffsetsRead<'a>
                        , pub terms:         &'a TermsRead<'a>
                        , pub postings:      &'a PostingsRead<'a>
                        , pub doc_offsets:   &'a DocOffsetsRead<'a>
                        , pub docs:          &'a DocsRead<'a>
                        }

pub fn with_index_read<A>(file_name: &str,
                          f:         &dyn Fn(IndexRead) -> A) -> A {
    let total_doc_len = get_total_doc_len(file_name);
    with_term_offsets(file_name, &|term_offsets_read| 
    with_terms       (file_name, &|terms_read| 
    with_postings    (file_name, &|postings_read|
    with_doc_offsets (file_name, &|doc_offsets_read|
    with_docs        (file_name, &|docs_read| {
        let ir = IndexRead { total_doc_len: total_doc_len 
                           , term_offsets:  &term_offsets_read
                           , terms:         &terms_read
                           , postings:      &postings_read
                           , doc_offsets:   &doc_offsets_read
                           , docs:          &docs_read
                           };
        f(ir)
    })))))
}

/* Binary search for a term */
pub fn find_term(ir: &IndexRead,
                 term: &Term) -> Option<TermEntry> {
    let TermOffsetsRead(term_offsets) = ir.term_offsets;
    match term_offsets.binary_search_by(|off| read_term_at(&ir.terms, *off as usize).cmp(term)) {
        Err(_)       => None,
        Ok(term_idx) => {
            let term = read_term_entry_at(&ir.terms, term_offsets[term_idx] as usize);
            Some(term)
        }
    }
}

pub fn find_doc(ir:     &IndexRead,
                doc_id: DocId) -> Option<DocEntry> {
    let DocOffsetsRead(doc_offsets) = ir.doc_offsets;
    match doc_offsets.binary_search_by(|off| read_doc_at(&ir.docs, *off as usize).doc_id.cmp(&doc_id)) {
        Err(_)       => None,
        Ok(doc_idx) => Some(read_doc_at(&ir.docs, doc_offsets[doc_idx] as usize))
    }
}

pub fn find_docid(DocOffsetsRead(offs): &DocOffsetsRead,
                  docs:                 &DocsRead,
                  doc_id:               DocId) -> bool {
    match offs.binary_search_by(|off| read_doc_at(docs, *off as usize).doc_id.cmp(&doc_id)) {
        Err(_) => false,
        Ok(_)  => true
    }
}

fn get_total_doc_len(file_name: &str) -> u64 {
    let mut contents = String::new();
    read_complete_file(&mut contents,
                       &format!("{}/totalDocLength", file_name)).expect(&format!("Could not open {}", file_name));
    contents.parse::<u64>().unwrap()
  }

// TODO library function instead?
pub fn read_complete_file(dest: &mut String,
                          file_name: &String) -> std::io::Result<()> {
    let file = File::open(file_name)?;
    let mut buf_reader = BufReader::new(file);
    buf_reader.read_to_string(dest)?;
    Ok(())
}

pub fn read_doc_at(DocsRead(docs): &DocsRead,
                   offset: usize) -> DocEntry {

        let mut i = offset;

        // TODO unescape url
        let mut url_buf = Vec::<u8>::new();
        while docs[i] != 0 {
            url_buf.push(docs[i]);
            i += 1;
        }
        let url = from_utf8(&url_buf).unwrap().to_string();

        // "\0"
        i += 1;

        // doc_id u32
        let (doc_id, read) = read_esc_u32_at(&docs, i);
        i += read;

        // doc_len u32
        let (doc_len, _) = read_esc_u32_at(&docs, i);

        // newline (not escaped)

    DocEntry { url:     Url(url)
             , doc_id:  DocId(doc_id)
             , doc_len: doc_len
             }
}

pub fn read_term_at(TermsRead(terms): &TermsRead,
                    offset: usize) -> Term {

    let mut i = offset;

    // TODO unescape term
    let mut term_buf = Vec::<u8>::new();
    while terms[i] != 0 {
        term_buf.push(terms[i]);
        i += 1;
    }
    let term = from_utf8(&term_buf).unwrap().to_string();
    Term(term)
}

pub fn read_term_entry_at(TermsRead(terms): &TermsRead,
                          offset: usize) -> TermEntry {

    let mut i = offset;

    // Term TODO unescape?
    let mut term_buf = Vec::<u8>::new();
    while terms[i] != 0 {
        term_buf.push(terms[i]);
        i += 1;
    }
    let term = Term(from_utf8(&term_buf).unwrap().to_string());

    // "\0"
    i += 1;

    // Offset
    let (off, read) = read_esc_u64_at(&terms, i);
    i += read;

    // Document Frequency
    let (df, _) = read_esc_u32_at(&terms, i);

    // Newline (not escaped) 
    // No action
    TermEntry { term:     term
              , offset:   off
              , doc_freq: df
              }
}

pub fn with_term_offsets<A>(idx_name: &str,
                            f:        &dyn Fn(TermOffsetsRead) -> A) -> A {
    with_vec_align(&format!("{}/termOffsets", idx_name),
                   &|to| f(TermOffsetsRead(to)))
}

pub fn with_terms<A>(idx_name: &str,
                     f:        &dyn Fn(TermsRead) -> A) -> A {
    with_vec(&format!("{}/terms", idx_name),
             &|t| f(TermsRead(t)))
}

pub fn with_postings<A>(idx_name: &str,
                        f:        &dyn Fn(PostingsRead) -> A) -> A {
    with_vec_align(&format!("{}/postings", idx_name),
                   &|p| f(PostingsRead(p)))
}

pub fn with_doc_offsets<A>(idx_name: &str,
                           f:        &dyn Fn(DocOffsetsRead) -> A) -> A {
    with_vec_align(&format!("{}/docOffsets", idx_name),
                   &|to| f(DocOffsetsRead(to)))
}

pub fn with_named_doc_offsets<A>(file_name: &str,
                                 f:        &dyn Fn(DocOffsetsRead) -> A) -> A {
    with_vec_align(file_name, &|to| f(DocOffsetsRead(to)))
}

pub fn with_doc_offsets_mut<A>(idx_name: &str,
                               f:        &mut dyn FnMut(DocOffsetsRead) -> A) -> A {
    with_vec_align_mut(&format!("{}/docOffsets", idx_name),
                       &mut|to| f(DocOffsetsRead(to)))
}

pub fn with_docs<A>(idx_name: &str,
                    f:        &dyn Fn(DocsRead) -> A) -> A {
    with_vec(&format!("{}/docs", idx_name),
             &|t| f(DocsRead(t)))
}

pub fn with_named_docs<A>(file_name: &str,
                          f:        &dyn Fn(DocsRead) -> A) -> A {
    with_vec(file_name, &|t| f(DocsRead(t)))
}

pub fn with_docs_mut<A>(idx_name: &str,
                        f:        &mut dyn FnMut(DocsRead) -> A) -> A {
    with_vec_mut(&format!("{}/docs", idx_name),
                 &mut|t| f(DocsRead(t)))
}

pub fn with_vec_align<A,V>(file_name: &str,
                           f:         &dyn Fn(&[V]) -> A) -> A {
    let file = File::open(file_name).expect(&format!("Could not open {}", file_name));
    let x: A;
    unsafe {
        let mmap = Mmap::map(&file).expect(&format!("failed to map u32/u64 file: {}", file_name));
        let (prefix, data, suffix) = mmap.align_to::<V>();
        debug_assert!(prefix.is_empty());
        debug_assert!(suffix.is_empty(), "non-empty suffix on: {}", file_name);
        x = f(data);
    };
    x
}

pub fn with_vec_align_mut<A,V>(file_name: &str,
                               f:         &mut dyn FnMut(&[V]) -> A) -> A {
    let file = File::open(file_name).expect(&format!("Could not open {}", file_name));
    let x: A;
    unsafe {
        let mmap = Mmap::map(&file).expect("failed to map u32/u64 file");
        let (prefix, data, suffix) = mmap.align_to::<V>();
        debug_assert!(prefix.is_empty());
        debug_assert!(suffix.is_empty());
        x = f(data);
    };
    x
}

pub fn with_vec<A>(file_name: &str,
                   f:         &dyn Fn(&[u8]) -> A) -> A {
    let file = File::open(file_name).expect(&format!("Could not open {}", file_name));
    let x: A;
    unsafe {
        let data = Mmap::map(&file).expect("failed to map u8 file");
        x = f(&data);
    };
    x
}

fn with_vec_mut<A>(file_name: &str,
                   f:         &mut dyn FnMut(&[u8]) -> A) -> A {
    let file = File::open(file_name).expect(&format!("Could not open {}", file_name));
    let x: A;
    unsafe {
        let data = Mmap::map(&file).expect("failed to map u8 file");
        x = f(&data);
    };
    x
}

pub fn with_mut_vec<A, V>(file_name: &str,
                          f:         &dyn Fn(&mut [V]) -> A) -> A {
    let file = OpenOptions::new().read(true).write(true).open(file_name).expect(&format!("Could not open {}", file_name));
    let x: A;
    unsafe {
        let mut mmap = MmapMut::map_mut(&file).expect("failed to map terms file");
        let (prefix, data, suffix) = mmap.align_to_mut::<V>();
        debug_assert!(prefix.is_empty());
        debug_assert!(suffix.is_empty());
        x = f(data);
    };
    x
}
