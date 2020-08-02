
use doc::*;
use merge::common_urls::*;
use merge::common_docids::*;
use merge::util::*;
use index_reader::*;
use terms::*;
use types::*;

use byteorder::{LittleEndian, WriteBytesExt};
use itertools::Itertools;
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Write};
use write_to_buf::WriteToBuf;

#[derive(Debug)]
struct DocIdRemapping<'a>
    { common_docids_path:         &'a str
    , common_docids_resolve_path: &'a str
    }

pub fn merge(idx_name_dest: &str,
             idx_name_a:    &str,
             idx_name_b:    &str) {

    // Prepare empty directory
    delete_recreate(idx_name_dest).unwrap();

    // Write out the common urls
    let docids_excluded_by_dupe_url = format!("{}/docidsExcludedByUrl", idx_name_dest);
    let common_urls = write_common_urls_ordered(idx_name_dest,
                                                idx_name_a,
                                                idx_name_b,
                                                &docids_excluded_by_dupe_url);

    // Write out the common docids
    let common_docids_path         = format!("{}/commonDocids", idx_name_dest);
    let common_docids_resolve_path = format!("{}/commonDocidsResolve", idx_name_dest);
    let common_docids = resolve_common_docids(idx_name_a,
                                              idx_name_b,
                                              &common_docids_path,
                                              &common_docids_resolve_path);

    // Remap on A side
    let docid_remapping =
        if common_docids {
            let docid_remapping =
                DocIdRemapping { common_docids_path:         &common_docids_path
                               , common_docids_resolve_path: &common_docids_resolve_path
                               };

            let post_a_remapped_path = format!("{}/postings_a_remapped", idx_name_dest);

            remap_a_postings(idx_name_a,
                             &post_a_remapped_path,
                             &docid_remapping);

            Some(docid_remapping)
        } else {
            None
        };

    // Dedupe on B side
    let docid_deduping =
        if common_urls {
            let term_offs_path = format!("{}/term_offs_b_deduped", idx_name_dest);
            let term_path      = format!("{}/terms_b_deduped",     idx_name_dest);
            let post_path      = format!("{}/postings_b_deduped",  idx_name_dest);
            dedupe_b_term_postings(idx_name_b,
                                   &docids_excluded_by_dupe_url,
                                   &term_offs_path,
                                   &term_path,
                                   &post_path);
            Some(docids_excluded_by_dupe_url)
        } else {
            None
        };

    combine_docs(idx_name_dest,
                 idx_name_a,
                 idx_name_b,
                 &docid_remapping,
                 &docid_deduping);

    final_merge(idx_name_dest,
                idx_name_a,
                idx_name_b,
                docid_remapping,
                docid_deduping);
}

fn final_merge(idx_name_dest:   &str,
               idx_name_a:      &str,
               idx_name_b:      &str,
               docid_remapping: Option<DocIdRemapping>,
               docid_deduping:  Option<String>) {

    // Decide terms

    let term_offs_a_path = format!("{}/termOffsets", idx_name_a);
    let terms_a_path     = format!("{}/terms",       idx_name_a);

    let (term_offs_b_path,
         terms_b_path) = 
        match docid_deduping {
            None    => (format!("{}/termOffsets", idx_name_b),
                        format!("{}/terms",       idx_name_b)),
            Some(_) => (format!("{}/term_offs_b_deduped", idx_name_dest),
                        format!("{}/terms_b_deduped",     idx_name_dest))
        };

    // Decide postings
    let posts_a_path = 
        match docid_remapping {
            None    => format!("{}/postings",            idx_name_a),
            Some(_) => format!("{}/postings_a_remapped", idx_name_dest)
        };

    let posts_b_path =
        match docid_deduping {
            None    => format!("{}/postings",            idx_name_b),
            Some(_) => format!("{}/postings_b_deduped",  idx_name_dest)
        };

    // If any zeroes in side B, don't merge, just copy side A
    let sz_b = fs::metadata(&term_offs_b_path).unwrap().len()
             * fs::metadata(    &terms_b_path).unwrap().len()
             * fs::metadata(    &posts_b_path).unwrap().len();

    if sz_b == 0 {

        for file in &[ "docOffsets"
                     , "docs"
                     , "postings"
                     , "termOffsets"
                     , "terms"
                     , "totalDocLength" ] {
            let src = format!("{}/{}", idx_name_a,    file);
            let dst = format!("{}/{}", idx_name_dest, file);
            fs::copy(src, dst).unwrap();
        }

    } else {

        with_vec_align(&term_offs_a_path, &|t_offs_a: &[u64]|
        with_vec      (&terms_a_path,     &|terms_a         |
        with_vec_align(&term_offs_b_path, &|t_offs_b: &[u64]|
        with_vec      (&terms_b_path,     &|terms_b         | 
        with_vec_align(&posts_a_path,     &|posts_a:  &[u32]| 
        with_vec_align(&posts_b_path,     &|posts_b:  &[u32]| {

            let mut dest_term_offs_writer =
                BufWriter::new(File::create(format!("{}/termOffsets", idx_name_dest)).unwrap());

            let mut dest_terms_writer =
                BufWriter::new(File::create(format!("{}/terms", idx_name_dest)).unwrap());

            let mut dest_postings_writer =
                BufWriter::new(File::create(format!("{}/postings", idx_name_dest)).unwrap());

            let iter_terms_a =
                t_offs_a.iter()
                        .map(|&off_a| read_term_entry_at(&TermsRead(terms_a), off_a as usize))
                        .map(|term_a| (term_a.term, vec![('a', term_a.offset as usize, term_a.doc_freq as usize)]));

            let iter_terms_b =
                t_offs_b.iter()
                        .map(|&off_b| read_term_entry_at(&TermsRead(terms_b), off_b as usize))
                        .map(|term_b| (term_b.term, vec![('b', term_b.offset as usize, term_b.doc_freq as usize)]));

            let mut term_off = 0;
            let mut post_off = 0;

            let mut total_doc_len = 0;

            for (term, meta) in iter_terms_a.merge(iter_terms_b)
                                            .coalesce( | (term_x, meta_x)
                                                       , (term_y, meta_y) | {
                                                           if term_x == term_y {
                                                               let a = meta_x[0];
                                                               let b = meta_y[0];
                                                               Ok((term_x, vec![a,b]))
                                                            } else {
                                                                Err(((term_x, meta_x), (term_y, meta_y)))
                                                            }}) {

                match meta[..] {

                    [ ('a', off_a, freq_a)
                    , ('b', off_b, freq_b) ] => { let iter_a = (off_a..off_a+freq_a).map(|x| (posts_a[2*x], posts_a[2*x+1]));
                                                  let iter_b = (off_b..off_b+freq_b).map(|x| (posts_b[2*x], posts_b[2*x+1]));

                                                  let mut num_posts = 0;
                                                  for (docid, freq) in iter_a.merge_by(iter_b, |a,b| a <= b) {
                                                      // Populate postings file
                                                      dest_postings_writer.write_u32::<LittleEndian>(docid).unwrap();
                                                      dest_postings_writer.write_u32::<LittleEndian>(freq).unwrap();
                                                      total_doc_len += freq;
                                                      num_posts += 1;
                                                  }

                                                  // Term
                                                  let term_entry = TermEntry { term:     term
                                                                             , offset:   post_off
                                                                             , doc_freq: num_posts};

                                                  post_off += num_posts as u64;
                                                  let mut buf = Vec::<u8>::new();
                                                  let written = term_entry.write_to_buf(&mut buf);
                                                  dest_terms_writer.write(&buf).unwrap();

                                                  // Term offset
                                                  dest_term_offs_writer.write_u64::<LittleEndian>(term_off).unwrap();
                                                  term_off += written as u64;
                                                },

                    [ ('a', off, freq) ] => { let mut num_posts = 0;
                                              for p in off .. (off + freq) {
                                                  let docid = posts_a[2 * p];
                                                  let freq  = posts_a[2 * p + 1];
                                                  // Populate postings file
                                                  dest_postings_writer.write_u32::<LittleEndian>(docid).unwrap();
                                                  dest_postings_writer.write_u32::<LittleEndian>(freq).unwrap();
                                                  total_doc_len += freq;
                                                  num_posts += 1;
                                              }

                                              // Term
                                              let term_entry = TermEntry { term:     term
                                                                         , offset:   post_off
                                                                         , doc_freq: num_posts};

                                              post_off += num_posts as u64;
                                              let mut buf = Vec::<u8>::new();
                                              let written = term_entry.write_to_buf(&mut buf);
                                              dest_terms_writer.write(&buf).unwrap();
                                              
                                              // Term offset
                                              dest_term_offs_writer.write_u64::<LittleEndian>(term_off).unwrap();
                                              term_off += written as u64;
                                            },

                    [ ('b', off, freq) ] => { let mut num_posts = 0;
                                              for p in off .. (off + freq) {
                                                  let docid = posts_b[2 * p];
                                                  let freq  = posts_b[2 * p + 1];
                                                  dest_postings_writer.write_u32::<LittleEndian>(docid).unwrap();
                                                  dest_postings_writer.write_u32::<LittleEndian>(freq).unwrap();
                                                  total_doc_len += freq;
                                                  num_posts += 1;
                                              }
                                              // Term
                                              let term_entry = TermEntry { term:     term
                                                                         , offset:   post_off
                                                                         , doc_freq: num_posts};

                                              post_off += num_posts as u64;
                                              let mut buf = Vec::<u8>::new();
                                              let written = term_entry.write_to_buf(&mut buf);
                                              dest_terms_writer.write(&buf).unwrap();
                     
                                              // Term offset
                                              dest_term_offs_writer.write_u64::<LittleEndian>(term_off).unwrap();
                                              term_off += written as u64;
                                            }

                    _ => panic!("Impossible!")
                };
            }

            let mut total_doc_len_file = File::create(format!("{}/totalDocLength", idx_name_dest)).unwrap();
            total_doc_len_file.write_all(format!("{}", total_doc_len).as_bytes()).unwrap();

            dest_term_offs_writer.flush().unwrap();
            dest_terms_writer.flush().unwrap();
            dest_postings_writer.flush().unwrap();
            }))))));
    }
}

fn dedupe_b_term_postings(idx_name_b:                  &str,
                          docids_excluded_by_dupe_url: &str,
                          term_offs_path:              &str,
                          term_path:                   &str,
                          post_path:                   &str) {

    with_vec_align(docids_excluded_by_dupe_url, &|exclude_docids: &[u32]|
    with_term_offsets(idx_name_b,               &|TermOffsetsRead(t_offs_b)|
    with_terms(idx_name_b,                      &|terms_b| 
    with_postings(idx_name_b,                   &|PostingsRead(posts_b)| {

        let mut term_offs_b_deduped_writer =
            BufWriter::new(File::create(&term_offs_path).unwrap());

        let mut terms_b_deduped_writer =
            BufWriter::new(File::create(&term_path).unwrap());

        let mut posts_b_deduped_writer =
            BufWriter::new(File::create(&post_path).unwrap());

        let mut out_term_off = 0;
        let mut out_post_off = 0;

        for &t_off in t_offs_b {
            let term_entry = read_term_entry_at(&terms_b, t_off as usize);
            let in_start =            term_entry.offset   as usize;
            let in_end   = in_start + term_entry.doc_freq as usize;
            let mut p = in_start;
            let mut post_count = 0;

            let mut posts_written = 0;

            while p < in_end {
                let docid = posts_b[2 * p];
                let freq  = posts_b[2 * p + 1];
                match exclude_docids.binary_search(&docid) {
                    Ok(_)  => (),
                    Err(_) => {                        
                        posts_b_deduped_writer.write_u32::<LittleEndian>(docid).unwrap();
                        posts_b_deduped_writer.write_u32::<LittleEndian>(freq).unwrap();
                        post_count += 1;
                        posts_written += 1;
                    }
                };
                p += 1;
            }

            if post_count > 0 {
                term_offs_b_deduped_writer.write_u64::<LittleEndian>(out_term_off).unwrap();
                let term_entry_out = TermEntry { term:     term_entry.term
                                               , offset:   out_post_off
                                               , doc_freq: post_count};
                let mut buf = Vec::<u8>::new();
                let written = term_entry_out.write_to_buf(&mut buf);
                terms_b_deduped_writer.write(&buf).unwrap();
                out_term_off += written as u64;
                out_post_off += posts_written;
            }
        }
        
        term_offs_b_deduped_writer.flush().unwrap();
        terms_b_deduped_writer.flush().unwrap();
        posts_b_deduped_writer.flush().unwrap();

    }))))
    // TODO remove paths above if not used (out_term_off>0 && out_post_off>0)
}

fn remap_a_postings(idx_name_a:           &str,
                    post_a_remapped_path: &str,
                    DocIdRemapping { common_docids_path,
                                     common_docids_resolve_path }: &DocIdRemapping) {

    // First pass: Just write out the (sometimes) remapped posts

    with_vec_align(common_docids_path,                  &|com_docids: &[u32]| 
    with_vec_align(common_docids_resolve_path,          &|com_resolv: &[u32]| 
    with_vec_align(&format!("{}/postings", idx_name_a), &|posts_a: &[u64]| {
        let mut post_a_remapped_writer =
            BufWriter::new(File::create(&post_a_remapped_path).unwrap());
        for post in posts_a {
            let docid =  *post        as u32;
            let freq  = (*post >> 32) as u32;
            let new_post =
                match com_docids.binary_search(&docid) {
                    Ok(i)  => ((freq as u64) << 32) | (com_resolv[i] as u64),
                    Err(_) => *post
                };
            post_a_remapped_writer.write_u64::<LittleEndian>(new_post).unwrap();
        }
        post_a_remapped_writer.flush().unwrap();
    })));

    // Second pass: sort each postings run
    with_term_offsets(idx_name_a,                     &|TermOffsetsRead(t_offs_a)|
    with_terms(idx_name_a,                            &|terms_a|        
    with_u64_mut_vec(&post_a_remapped_path, None, &mut |posts_a_remapped| 
        t_offs_a.iter()
                .map(|t_off_a| read_term_entry_at(&terms_a, *t_off_a as usize))
                .for_each(|term_entry| {

                    let p =     term_entry.offset   as usize;
                    let q = p + term_entry.doc_freq as usize; 

                    posts_a_remapped[p..q].sort_by(&|x:&u64,y:&u64| {
                        let x_docid = *x as u32;
                        let y_docid = *y as u32;
                        x_docid.cmp(&y_docid)
                    });
                })
    )))
}

fn combine_docs(idx_name_dest:   &str,
                idx_name_a:      &str,
                idx_name_b:      &str,
                docid_remapping: &Option<DocIdRemapping>,
                docid_deduping:  &Option<String>) {

        let docs_a_remapped_path     = format!("{}/docsARemapped", idx_name_dest);
        let doc_offs_a_remapped_path = format!("{}/docsARemappedOffsets", idx_name_dest);

        // Prepare A documents
        match docid_remapping {

                // Do nothing, (read from original docs_a)
                None => (),

                // A shall remap its docids on collision
                Some(docid_remapping) =>
                    with_doc_offsets(idx_name_a, &|offs_a|
                    with_docs(       idx_name_a, &|docs_a| {
                        prepare_a_documents(&offs_a,
                                            &docs_a,
                                            &docs_a_remapped_path,
                                            &doc_offs_a_remapped_path,
                                            docid_remapping)
                    }))
        }

        // Iterate from original A or remapped A if there were collisions
        let (docs_a_path, doc_offsets_a_path) = 
            match docid_remapping {
                Some(_) => (docs_a_remapped_path, doc_offs_a_remapped_path),
                None    => (format!("{}/docs", idx_name_a),format!("{}/docOffsets", idx_name_a))
            };

        // Iterate-merge over (offsa/offsremapa) & offsb
        // to produce final docs and docOffsets
        with_named_doc_offsets(&doc_offsets_a_path, &|offs_a|
        with_named_docs(       &docs_a_path,        &|docs_a| 
        with_doc_offsets(      idx_name_b,          &|offs_b|
        with_docs(             idx_name_b,          &|docs_b| {

            match docid_deduping {

                // Merge (possibly remapped) A and B as is.
                None => simple_merge(idx_name_dest,
                                     &offs_a,
                                     &docs_a,
                                     &offs_b,
                                     &docs_b),

                // Merge (possibly remapped) A and B (excluding duplicates from B)
                Some(docid_deduping) => dedupe_merge(idx_name_dest,
                                                     &offs_a,
                                                     &docs_a,
                                                     &offs_b,
                                                     &docs_b,
                                                     docid_deduping)
            }
        }))));
}

// Merge (possibly remapped) A and B as is.
fn simple_merge(idx_name_dest:          &str,
                DocOffsetsRead(offs_a): &DocOffsetsRead,
                docs_a:                 &DocsRead,
                DocOffsetsRead(offs_b): &DocOffsetsRead,
                docs_b:                 &DocsRead) {

    let iter_a =
            offs_a.iter().map(|off_a| read_doc_at(&docs_a, *off_a as usize));

    let iter_b =
            offs_b.iter().map(|off_b| read_doc_at(&docs_b, *off_b as usize));

    let merged =
            iter_a.merge_by(iter_b, |doc_a, doc_b| {
                let DocId(di_a) = doc_a.doc_id;
                let DocId(di_b) = doc_b.doc_id;
                di_a <= di_b});

    let dest_doc_offsets_path = format!("{}/docOffsets", idx_name_dest);
    let dest_docs_path =        format!("{}/docs", idx_name_dest);
    let mut dest_doc_offsets_writer = BufWriter::new(File::create(&dest_doc_offsets_path).unwrap());
    let mut dest_docs_writer        = BufWriter::new(File::create(&dest_docs_path).unwrap());

    let mut offset = 0;

    for doc in merged {
        dest_doc_offsets_writer.write_u64::<LittleEndian>(offset as u64).unwrap();
        let mut buf = Vec::<u8>::new();
        let written = doc.write_to_buf(&mut buf);
        dest_docs_writer.write(&buf).unwrap();
        offset += written;
    }

    dest_doc_offsets_writer.flush().unwrap();
    dest_docs_writer.flush().unwrap();
}

// Merge (possibly remapped) A and B (excluding duplicates from B)
fn dedupe_merge(idx_name_dest:          &str,
                DocOffsetsRead(offs_a): &DocOffsetsRead,
                docs_a:                 &DocsRead,
                DocOffsetsRead(offs_b): &DocOffsetsRead,
                docs_b:                 &DocsRead,
                dupe_docids_b_path:     &str) {

    with_vec_align(dupe_docids_b_path,
                   &|dupe_b: &[u32]| {

        let iter_a =
            offs_a.iter()
                  .map(|off_a| read_doc_at(&docs_a, *off_a as usize));
        
        let iter_b =
            offs_b.iter()
                  .map(|off_b| read_doc_at(&docs_b, *off_b as usize))
                  .filter(|doc_b| {
                    let DocId(di_b) = doc_b.doc_id;
                    match dupe_b.binary_search(&di_b) {
                        Ok(_) => false,
                        Err(_) => true
                    }});

        let merged =
            iter_a.merge_by(iter_b, |doc_a, doc_b| {
                let DocId(di_a) = doc_a.doc_id;
                let DocId(di_b) = doc_b.doc_id;
                di_a <= di_b});

        let dest_doc_offsets_path = format!("{}/docOffsets", idx_name_dest);
        let dest_docs_path =        format!("{}/docs", idx_name_dest);
        let mut dest_doc_offsets_writer = BufWriter::new(File::create(&dest_doc_offsets_path).unwrap());
        let mut dest_docs_writer        = BufWriter::new(File::create(&dest_docs_path).unwrap());

        let mut offset = 0;

        for doc in merged {
            dest_doc_offsets_writer.write_u64::<LittleEndian>(offset as u64).unwrap();
            let mut buf = Vec::<u8>::new();
            let written = doc.write_to_buf(&mut buf);
            dest_docs_writer.write(&buf).unwrap();
            offset += written;
        }

        dest_doc_offsets_writer.flush().unwrap();
        dest_docs_writer.flush().unwrap();
    });
}

fn prepare_a_documents(DocOffsetsRead(offs_a):   &DocOffsetsRead,
                       docs_a:                   &DocsRead,
                       docs_a_remapped_path:     &str,
                       doc_offs_a_remapped_path: &str,
                       DocIdRemapping { common_docids_path,
                                        common_docids_resolve_path }: &DocIdRemapping) {

    with_u32_mut_vec(common_docids_path,         None, &mut |com_docids|
    with_u32_mut_vec(common_docids_resolve_path, None, &mut |com_resolv| {

        // write out new docs_a_tmp
        let mut docs_a_remapped_writer =
            BufWriter::new(File::create(&docs_a_remapped_path).unwrap());

        // need an offsets file so it can be sort_by(docid)
        let mut doc_offs_a_remapped_writer =
            BufWriter::new(File::create(&doc_offs_a_remapped_path).unwrap());

        let mut offset = 0;

        // For each doc in a
        for &off_a in offs_a.iter() {
            let doc_entry_a = read_doc_at(docs_a, off_a as usize);
            let DocId(di) = doc_entry_a.doc_id;

            // Remap it if necessary
            let doc_entry_a_rm = 
                match com_docids.binary_search(&di) {
                    Ok(p)  => DocEntry { doc_id:  DocId(com_resolv[p])
                                       , doc_len: doc_entry_a.doc_len
                                       , url:     doc_entry_a.url
                                       },
                    Err(_) => doc_entry_a
                };

            // Write the offset
            doc_offs_a_remapped_writer.write_u64::<LittleEndian>(offset as u64).unwrap();

            // Write it out
            let mut buf = Vec::<u8>::new();
            let written = doc_entry_a_rm.write_to_buf(&mut buf);
            docs_a_remapped_writer.write(&buf).unwrap();
            offset += written;
        }

        docs_a_remapped_writer.flush().unwrap();
        doc_offs_a_remapped_writer.flush().unwrap();
    }));

    // Sort the offsets to reflect the new docid order
    with_vec        (&docs_a_remapped_path,           &    |docs_a_remapped| 
    with_u64_mut_vec(&doc_offs_a_remapped_path, None, &mut |offs_a_remapped| 
        offs_a_remapped.sort_by(|&x, &y| {
            let doc_id_x = read_doc_at(&DocsRead(docs_a_remapped), x as usize).doc_id;
            let doc_id_y = read_doc_at(&DocsRead(docs_a_remapped), y as usize).doc_id;
            doc_id_x.cmp(&doc_id_y)
        })
    ));
}