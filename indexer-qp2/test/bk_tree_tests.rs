#[cfg(test)]
use crate::util::fresh_tmp_dir;
#[cfg(test)]
use indexer_qp2::bk_tree::*;
#[cfg(test)]
use std::collections::HashSet;

#[cfg(test)]
#[test]
fn test_ram_no_match() {
    let bk_tree = new_bk_tree("too".to_string());

    assert_eq!(
        query_bk_set(&bk_tree, &"different".to_string(), 0),
        HashSet::new()
    );
}

#[cfg(test)]
#[test]
fn test_ram_exact_match() {
    let mut bk_tree = new_bk_tree("one".to_string());
    insert_bk(&mut bk_tree, "two".to_string());
    insert_bk(&mut bk_tree, "three".to_string());
    insert_bk(&mut bk_tree, "four".to_string());
    insert_bk(&mut bk_tree, "five".to_string());

    assert_eq!(
        query_bk_set(&bk_tree, &"one".to_string(), 0),
        HashSet::from([(0, "one".to_string())])
    );

    assert_eq!(
        query_bk_set(&bk_tree, &"four".to_string(), 0),
        HashSet::from([(0, "four".to_string())])
    );
}

#[cfg(test)]
#[test]
fn test_ram_partial_match() {
    let mut bk_tree = new_bk_tree("one".to_string());
    insert_bk(&mut bk_tree, "two".to_string());
    insert_bk(&mut bk_tree, "three".to_string());
    insert_bk(&mut bk_tree, "four".to_string());
    insert_bk(&mut bk_tree, "five".to_string());

    assert_eq!(
        query_bk_set(&bk_tree, &"thr".to_string(), 2),
        HashSet::from([(2, "three".to_string()), (2, "two".to_string())])
    );
}

#[cfg(test)]
#[test]
fn test_disk_no_match() {
    let bk_tree = new_bk_tree("too".to_string());

    let fp = fresh_tmp_dir("test_disk_no_match_");

    let path = format!("{}/bk_tree", fp.display());
    write_to_disk(&path, &bk_tree);

    assert_eq!(
        query_disk_bk_set(&path, &"different".to_string(), 0),
        HashSet::new()
    );
}

#[cfg(test)]
#[test]
fn test_disk_exact_match() {
    let mut bk_tree = new_bk_tree("one".to_string());
    insert_bk(&mut bk_tree, "two".to_string());
    insert_bk(&mut bk_tree, "three".to_string());
    insert_bk(&mut bk_tree, "four".to_string());
    insert_bk(&mut bk_tree, "five".to_string());

    let fp = fresh_tmp_dir("test_disk_exact_match_");

    let path = format!("{}/bk_tree", fp.display());
    write_to_disk(&path, &bk_tree);

    assert_eq!(
        query_disk_bk_set(&path, &"one".to_string(), 0),
        HashSet::from([(0, "one".to_string())])
    );

    assert_eq!(
        query_disk_bk_set(&path, &"four".to_string(), 0),
        HashSet::from([(0, "four".to_string())])
    );
}

#[cfg(test)]
#[test]
fn test_disk_partial_match() {
    let mut bk_tree = new_bk_tree("one".to_string());
    insert_bk(&mut bk_tree, "two".to_string());
    insert_bk(&mut bk_tree, "three".to_string());
    insert_bk(&mut bk_tree, "four".to_string());
    insert_bk(&mut bk_tree, "five".to_string());

    let fp = fresh_tmp_dir("test_disk_partial_match_");

    let path = format!("{}/bk_tree", fp.display());
    write_to_disk(&path, &bk_tree);

    assert_eq!(
        query_disk_bk_set(&path, &"thr".to_string(), 2),
        HashSet::from([(2, "three".to_string()), (2, "two".to_string())])
    );
}
