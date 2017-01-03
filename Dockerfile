FROM haskell:7.10.3

WORKDIR /tmp

RUN apt-get update && apt-get install -y \
    wget \
    cmake \
    python \
    libblas-dev \
    liblapack-dev
RUN wget http://llvm.org/releases/3.5.2/llvm-3.5.2.src.tar.xz
RUN tar -xf llvm-3.5.2.src.tar.xz

WORKDIR llvm-3.5.2.src
RUN mkdir build
WORKDIR build
RUN cmake ..
RUN make -j5
RUN make install


RUN apt-get update && apt-get install -y \
    git
# WORKDIR subhask

# RUN stack test --bench

# WORKDIR /tmp
# WORKDIR HLearn
# RUN cabal update
# RUN cabal install --only-dependencies
# RUN cabal build


COPY deep-space /src
WORKDIR /src/deep-space
RUN stack build

# Add just the .cabal file to capture dependencies
# COPY ./snap-example.cabal /opt/server/snap-example.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
# RUN cabal install --only-dependencies -j4

# Add and Install Application Code
# RUN cabal install

# CMD ["stack", "exec", "deep-space-exe"]
# ENTRYPOINT ["/bin/bash"]