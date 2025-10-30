use crate::edit_distance::edit_distance;
use crate::index_reader::with_vec_mut;
use crate::index_writer::with_mut_bytes;
use intmap::IntMap;
use std::mem::size_of;
use std::str::from_utf8;

/*
    Disk-structure:

    term         : bytes
    \0           : byte
    num_children : u32
    [ dist        : u32
    , disk_offset : usize ]
*/

pub struct BkTree {
    term: String,
    children: IntMap<u64, BkTree>,
}

pub fn write_to_disk(file_name: &str, tree: &BkTree) {
    let disk_sz = bk_tree_size(tree);
    with_mut_bytes(file_name, disk_sz as u64, &mut |bytes| {
        write_bk_tree_to_bytes(0, bytes, tree)
    });
}

fn write_bk_tree_to_bytes(pos: usize, bytes: &mut [u8], tree: &BkTree) -> usize {
    let mut m_pos = pos;

    // Term
    for &b in tree.term.as_bytes() {
        bytes[m_pos] = b;
        m_pos = m_pos + 1;
    }

    // Zero
    bytes[m_pos] = 0;
    m_pos = m_pos + 1;

    // Number of children
    let num_children = tree.children.len() as u32;
    for b in num_children.to_le_bytes() {
        bytes[m_pos] = b;
        m_pos = m_pos + 1;
    }

    let mut revisit_child_offset_indexes = Vec::new();

    // For each child
    tree.children.iter().for_each(|(dist64, _)| {
        // u32 edit dist
        let dist = dist64 as u32;
        for b in dist.to_le_bytes() {
            bytes[m_pos] = b;
            m_pos = m_pos + 1;
        }

        // Remember where to come back and write the offset
        revisit_child_offset_indexes.push(m_pos);

        // leave space for a usize disk offset
        m_pos = m_pos + size_of::<usize>();
    });

    let mut revisit_pos = 0;

    // Write out each child
    tree.children.iter().for_each(|(_, child_tree)| {
        let mut start_write = revisit_child_offset_indexes[revisit_pos];

        for b in m_pos.to_le_bytes() {
            bytes[start_write] = b;
            start_write = start_write + 1;
        }

        revisit_pos = revisit_pos + 1;
        m_pos = m_pos + write_bk_tree_to_bytes(m_pos, bytes, child_tree);
    });

    let written = m_pos - pos;
    written
}

fn bk_tree_size(tree: &BkTree) -> usize {
    let front_sz = tree.term.len()       // Term length
                 + 1                     // \0
                 + size_of::<u32>()      // u32 num children
                 + tree.children.len() * // per child:
                   ( size_of::<u32>()        // u32 edit dist
                   + size_of::<usize>()      // usize offset of child
                   );

    front_sz
        + tree
            .children
            .iter()
            .map(|(_, child)| bk_tree_size(child))
            .sum::<usize>()
}

pub fn new_bk_tree(root: String) -> BkTree {
    BkTree {
        term: root,
        children: IntMap::new(),
    }
}

pub fn insert_bk(tree: &mut BkTree, term: String) -> () {
    let dist = edit_distance(&tree.term, &term) as u64;
    if dist == 0 {
        return;
    }
    match tree.children.get_mut(dist) {
        None => {
            tree.children.insert(dist, new_bk_tree(term));
        }
        Some(child) => {
            insert_bk(child, term);
        }
    }
}

pub fn query_disk_bk(
    file_name: &str,
    term: &String,
    thresh: i64,
    action: &mut dyn FnMut(i64, &String),
) {
    with_vec_mut(file_name, &mut |bytes| {
        query_bytes_bk(0, bytes, term, thresh, action)
    })
}

fn query_bytes_bk(
    pos: usize,
    bytes: &[u8],
    term: &String,
    thresh: i64,
    action: &mut dyn FnMut(i64, &String),
) {
    let term_len = bytes[pos..].iter().take_while(|&&b| b != 0).count();
    let tree_term = from_utf8(&bytes[pos..pos + term_len]).unwrap();
    let dist = edit_distance(tree_term, &term) as i64;

    if dist <= thresh {
        action(dist, &tree_term.to_string());
    }

    let min_thresh: i64 = dist - thresh;
    let max_thresh: i64 = dist + thresh;

    // Skip past the term
    let mut new_pos = pos + term_len + 1;

    // Read num_children
    let mut num_children_buf: [u8; 4] = Default::default();
    num_children_buf.copy_from_slice(&bytes[new_pos..new_pos + 4]);
    let num_children = u32::from_le_bytes(num_children_buf);
    new_pos = new_pos + size_of::<u32>();

    // Read each child and decide whether to visit
    let mut child_dist_buf: [u8; 4] = Default::default();
    let mut child_off_buf: [u8; 8] = Default::default();
    for _ in 0..num_children {
        child_dist_buf.copy_from_slice(&bytes[new_pos..new_pos + 4]);
        new_pos = new_pos + 4;
        let child_dist = u32::from_le_bytes(child_dist_buf) as i64;

        child_off_buf.copy_from_slice(&bytes[new_pos..new_pos + 8]);
        new_pos = new_pos + 8;

        if child_dist >= min_thresh && child_dist <= max_thresh {
            let child_off = usize::from_le_bytes(child_off_buf);
            query_bytes_bk(child_off, bytes, term, thresh, action);
        }
    }
}

// Query an in-memory bk tree
// Likely not used since it should be disk-based
fn _query_bk(tree: &BkTree, term: &String, thresh: i64, action: &mut dyn FnMut(i64, &String)) {
    let dist = edit_distance(&tree.term, &term) as i64;

    if dist <= thresh {
        action(dist, &tree.term);
    }

    let min_thresh: i64 = dist - thresh;
    let max_thresh: i64 = dist + thresh;

    for (child_dist_u64, child_tree) in tree.children.iter() {
        let child_dist = child_dist_u64 as i64;
        if child_dist >= min_thresh && child_dist <= max_thresh {
            _query_bk(child_tree, term, thresh, action);
        }
    }
}
