use deletions::*;
use index::*;
use index_reader::*;
use spelling_correction::mk_spell_correction;
use terms::*;
use types::*;

use byteorder::{LittleEndian, WriteBytesExt};
use std::fs::*;
use std::io::{BufWriter, Seek, SeekFrom, Write};

pub fn write_files(idx_dir: &String,
                   idx:     Index) -> () {

    create_dir_all(idx_dir).unwrap();

    {
        let mut total_doc_len_file = BufWriter::new(File::create(format!("{}/{}", idx_dir, "totalDocLength")).unwrap());
        total_doc_len_file.write_all(format!("{}", idx.total_doc_len).as_bytes()).unwrap();
    }

    {
        let mut term_offsets_file = BufWriter::new(File::create(format!("{}/{}", idx_dir, "termOffsets")).unwrap());
        let TermOffsets(term_offsets) = idx.term_offsets;
        for &t_o in term_offsets {
            term_offsets_file.write_u64::<LittleEndian>(t_o).unwrap();
        }
        term_offsets_file.flush().unwrap();
    }

    {
        let mut terms_file = BufWriter::new(File::create(format!("{}/{}", idx_dir, "terms")).unwrap());
        let Terms(Escaped(t)) = idx.terms;
        terms_file.write_all(&t).unwrap();
        terms_file.flush().unwrap();
    }

    {
        let mut postings_file = BufWriter::new(File::create(format!("{}/{}", idx_dir, "postings")).unwrap());
        let Postings(postings) = idx.postings;
        for &p in postings {
            postings_file.write_u32::<LittleEndian>(p).unwrap();
        }
        postings_file.flush().unwrap();
    }

    {
        let mut doc_offsets_file = BufWriter::new(File::create(format!("{}/{}", idx_dir, "docOffsets")).unwrap());
        let DocOffsets(doc_offsets) = idx.doc_offsets;
        for &d_o in doc_offsets {
            doc_offsets_file.write_u64::<LittleEndian>(d_o).unwrap();
        }
        doc_offsets_file.flush().unwrap();
    }

    {
        let mut docs_file = BufWriter::new(File::create(format!("{}/{}", idx_dir, "docs")).unwrap());
        let Docs(Escaped(d)) = idx.docs;
        docs_file.write_all(&d).unwrap();
        docs_file.flush().unwrap();
    }

    {
        let DocOffsets(doc_offsets) = idx.doc_offsets;
        write_empty_deletions_file(doc_offsets.len(), File::create(format!("{}/{}", idx_dir, "docDeletions")).unwrap());
    }

    {
        with_term_offsets(idx_dir, &|term_offs|
        with_terms(       idx_dir, &|terms| {
            let duration = mk_spell_correction(idx_dir, &term_offs, &terms);
            eprintln!("mk_spell_correction took {}", duration.as_millis());
        }));
    }
}

pub fn with_mut_bytes<A>(file_name: &str,
                         disk_sz:   u64,
                         f:         &dyn Fn(&mut [u8]) -> A) -> A {

    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(file_name)
        .unwrap();

    file.seek(SeekFrom::Start(disk_sz)).unwrap();
    file.write_all(&[0]).unwrap();

    unsafe {
        f(&mut memmap::MmapOptions::new().map_mut(&file).unwrap())
    }
}
