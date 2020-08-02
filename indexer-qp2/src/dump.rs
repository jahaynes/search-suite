
use index_reader::*;
use terms::*;
use types::*;

pub fn dump(index_path: &str) {

    println!("Dumping: {}", index_path);
    with_index_read(index_path, &|ir| {

        let IndexRead{ term_offsets
                     , terms
                     , postings
                     , doc_offsets: _
                     , docs: _
                     , total_doc_len: _ } = ir;

        dump_terms(term_offsets,
                   terms,
                   postings);
    });
    println!("");
}

fn dump_terms(TermOffsetsRead(offs): &TermOffsetsRead,
              terms_read:            &TermsRead,
              PostingsRead(posts):   &PostingsRead) {

    offs.iter()
        .map(&|off: &u64| read_term_entry_at(terms_read, *off as usize))
        .for_each(|term| {
            println!("{:?}", term);
            let start =         term.offset   as usize;
            let end   = start + term.doc_freq as usize;
            for i in start..end {
                println!("\t{} {}", posts[2*i], posts[2*i+1]);
            }
            println!("");
        });
}