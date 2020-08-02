use bytes::*;
use doc::*;
use index_reader::*;
use merge::util::prepare_empty_file;
use types::*;

use byteorder::{LittleEndian, WriteBytesExt};
use std::fs;
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Error, Write};
use std::process::Command;

struct CommonUrlWriter(BufWriter<File>);

fn new_common_url_writer(path: &str) -> CommonUrlWriter {
    prepare_empty_file(path);
    let file =
        OpenOptions::new()
                    .append(true)
                    .open(path)
                    .expect("Could not open for CommonUrlWriter!");
    CommonUrlWriter(BufWriter::new(file))
}

/* url | \0 | "a" | esc(docid) \n */
fn write_doc_entry(common_url_writer: &mut CommonUrlWriter,
                    side:             &str,
                    doc_entry:        DocEntry) {
    let CommonUrlWriter(writer) = common_url_writer;
    let Url(u)    = doc_entry.url;
    let DocId(di) = doc_entry.doc_id;

    // Url
    writer.write(u.as_bytes()).unwrap();

    // \0
    writer.write(b"\0").unwrap();

    // Side - 'a' or 'b'
    writer.write(side.as_bytes()).unwrap();

    // escaped DocId
    let mut di_buf = Vec::<u8>::new();
    write_esc_u32(&mut di_buf, di);
    writer.write(&di_buf).unwrap();

    // \n
    writer.write(b"\n").unwrap();
}

fn flush(common_url_writer: &mut CommonUrlWriter) -> Result<(),Error> {
    common_url_writer.0.flush()
}


/* url | \0 | "a" | esc(docid) \n */
fn docids_excluded_by_url(docs_by_url: &str,
                          docids_excluded_by_dupe_url: &str) -> bool {

    let at_least_one_dupe = 
        with_vec(docs_by_url, &|vec| {
            let mut at_least_one_dupe = false;
            let mut writer = BufWriter::new(File::create(docids_excluded_by_dupe_url).unwrap());
            let mut i = 0;
            let mut last_uri = Vec::<u8>::new();
            while i < vec.len() {

                let mut buf = Vec::<u8>::new();

                // Read Url (todo extract this)
                loop {
                    let c = vec[i];
                    i += 1;
                    if c == 0 {
                        break;
                    } else {
                        buf.push(c);
                    }
                }

                // Side
                let _side = vec[i] as char;
                i += 1;

                // DocId
                let (di, read) = read_esc_u32_at(vec, i);
                i += read + 1; // \n

                if buf == last_uri {
                    at_least_one_dupe = true;
                    writer.write_u32::<LittleEndian>(di).unwrap();
                }
                last_uri = buf;
            }
            writer.flush().unwrap();
            at_least_one_dupe
        });

    // Make sure the docids are sorted
    if at_least_one_dupe {
        with_mut_vec(docids_excluded_by_dupe_url, &mut |vec: &mut [u32]| 
            vec.sort()
        );
    }
    at_least_one_dupe
}

pub fn write_common_urls_ordered(idx_name_dest:               &str,
                                 idx_name_a:                  &str,
                                 idx_name_b:                  &str,
                                 docids_excluded_by_dupe_url: &str) -> bool {

    let docs_by_url = format!("{}/docsByUrl", idx_name_dest);

    {
        let mut common_url_writer = new_common_url_writer(&docs_by_url);
        copy_urls_from_index_to_file(&mut common_url_writer, &idx_name_a, "a");
        copy_urls_from_index_to_file(&mut common_url_writer, &idx_name_b, "b");
        flush(&mut common_url_writer).unwrap();
    }

    // Sort urls in place
    Command::new("sort")
            .env("LC_ALL", "C")
            .arg("-o")
            .arg(&docs_by_url)
            .arg(&docs_by_url)
            .output()
            .expect("failed to execute sort in detect_gather_collided_urls");

    let at_least_one_dupe =
        docids_excluded_by_url(&docs_by_url, &docids_excluded_by_dupe_url);

    fs::remove_file(docs_by_url).unwrap();    
    at_least_one_dupe
}

fn copy_urls_from_index_to_file(common_url_writer: &mut CommonUrlWriter,
                                idx_name:          &str, 
                                side:              &str) {
    with_doc_offsets_mut(idx_name, &mut|DocOffsetsRead(offs)| 
    with_docs_mut(       idx_name, &mut|docs                | {
        for &off in offs {
            write_doc_entry(common_url_writer, side, read_doc_at(&docs, off as usize));
        }
    }));
}
