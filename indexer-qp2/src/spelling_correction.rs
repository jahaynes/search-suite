use crate::bk_tree::*;
use crate::terms::*;
use std::collections::{HashMap, HashSet};
use std::str;
use std::time::{Duration, SystemTime};

pub fn query_bk(index_path: &str,
                term:       &String,
                max_dist:   i64) -> HashMap<i64, HashSet<String>> {

    /* TODO:
        Need some intelligence here.
        Get only the best scored terms?
        Make threshold proportional to word length?
    */

    let file_name = format!("{}/spelling", index_path);

    let mut near_matches : HashMap<i64, HashSet<String>> = HashMap::new();

    query_disk_bk(&file_name, term, max_dist, &mut |dist, near_term| {
        near_matches
            .entry(dist)
            .and_modify(|near_terms| {
                near_terms.insert(near_term.into());
            })
            .or_insert({
                let mut set = HashSet::new();
                set.insert(near_term.into());
                set
            });
        }
    );

    near_matches
}

// TODO haven't tested escaping
pub fn mk_spell_correction(index_path:   &str,
                           term_offsets: &TermOffsetsRead,
                           terms:        &TermsRead) -> Duration {

    let start_time = SystemTime::now();

    let TermOffsetsRead(tos) = term_offsets;
    let TermsRead(ts) = terms;

    // Create the BK tree with the first term
    let first_len = ts.into_iter().take_while(|&&b| b != 0).count();
    let first_term = str::from_utf8(&ts[0..first_len]).unwrap();
    let mut bk_tree = new_bk_tree(String::from(first_term));

    // Insert the rest of the terms
    for o in tos.iter().skip(1) {
        let start = *o as usize;
        let len = ts[start..]
                    .into_iter()
                    .take_while(|&&b| b != 0)
                    .count();
        let term = str::from_utf8(&ts[start..start+len]).unwrap();
        let str = String::from(term);
        insert_bk(&mut bk_tree, str);
    }

    let file_name = format!("{}/spelling", index_path);
    write_to_disk(&file_name, &bk_tree);

    SystemTime::now().duration_since(start_time).unwrap()
}
