
FROM haskell:8.8.3

WORKDIR /run

COPY --from=indexer-qp2 /usr/local/cargo/bin/indexer-qp2 .

COPY --from=searchapi3 /usr/src/haskell/searchapi3 .

ENV COLLECTIONS_DIR=/run/collections
ENV INDEXER_BINARY=/run/indexer-qp2

CMD ["./searchapi3", "+RTS", "-N16"]