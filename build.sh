#!/bin/bash

# Build the indexer/query processor
docker build indexer-qp2 --tag indexer-qp2

# Build the api
git submodule update --init --recursive searchapi3/WebArchive/
docker build searchapi3 --tag searchapi3

# Build the crawler
# docker build crawler3 --tag crawler3

# Build the deployment
docker build deploy --tag deploy
