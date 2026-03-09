#!/bin/bash
cargo llvm-cov --lcov --output-path ./target/lcov.info

# Requires
#   cargo install cargo-llvm-cov

# Uses vscode plugins:
#   rust-analyzer
#   Coverage Gutters
#     ctrl-shift-7 to view coverage
