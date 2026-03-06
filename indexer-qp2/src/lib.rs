//! indexer-qp2 library
//!
//! This library provides the core functionality for the search indexer.

pub mod bk_tree;
pub mod bytes;
pub mod deletions;
pub mod doc;
pub mod dump;
pub mod index;
pub mod index_reader;
pub mod index_writer;
pub mod input;
pub mod merge;
pub mod normalise;
pub mod query;
pub mod ranking;
pub mod spelling_correction;
pub mod terms;
pub mod types;
pub mod verify;
pub mod write_to_buf;

