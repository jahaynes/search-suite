FROM rust:1.45

WORKDIR /usr/src
RUN rustup target add x86_64-unknown-linux-musl
RUN USER=root cargo new indexer-qp2

WORKDIR /usr/src/indexer-qp2
COPY Cargo.toml ./
RUN cargo build --release

COPY src ./src
RUN cargo install --target x86_64-unknown-linux-musl --path .
