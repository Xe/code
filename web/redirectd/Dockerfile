FROM haskell:7.10.2

ADD ./redirectd.cabal /app/redirectd.cabal
WORKDIR /app
RUN cabal update && cabal install --only-dependencies -j

ADD . /app
RUN cabal update && cabal install

ENV PATH /root/.cabal/bin:$PATH

EXPOSE 8080
CMD redirectd
