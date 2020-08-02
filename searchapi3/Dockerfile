FROM haskell:8.8.3

RUN cabal v2-update

WORKDIR /usr/src/haskell

COPY WebArchive ./WebArchive

COPY searchapi3.cabal .

RUN cabal v2-build --dependencies-only

COPY CHANGELOG.md .
COPY LICENSE      .
COPY src          ./src

RUN cabal v2-build searchapi3
RUN cp `find . -type f -name searchapi3` .
