FROM haskell:8.8.4

RUN cabal v2-update

WORKDIR /usr/src/haskell

COPY crawler3.cabal .

RUN cabal v2-build --dependencies-only

COPY CHANGELOG.md .
COPY LICENSE      .
COPY src          ./src

RUN cabal v2-build crawler3
RUN cp `find . -type f -name crawler3` .
