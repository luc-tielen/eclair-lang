FROM ubuntu:20.04

# install packages
RUN echo 'tzdata tzdata/Areas select Europe' | debconf-set-selections
RUN echo 'tzdata tzdata/Zones/Europe select Paris' | debconf-set-selections
RUN apt-get update \
    && apt-get autoremove -y \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y \
       lsb-release wget software-properties-common gnupg \
       nodejs curl bison build-essential clang cmake \
       doxygen flex g++ git libffi-dev libncurses5-dev \
       libsqlite3-dev make mcpp python3 sqlite zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# install llvm 14
RUN mkdir -p /tmp/llvm-dir \
    && cd /tmp/llvm-dir \
    && wget https://apt.llvm.org/llvm.sh \
    && chmod +x llvm.sh \
    && ./llvm.sh 14 \
    && cd /tmp \
    && rm -rf /tmp/llvm-dir

# install ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN echo "source /root/.ghcup/env" >> ~/.bashrc

# install and set ghc
SHELL ["/bin/bash", "-c", "source /root/.ghcup/env"] 
RUN ghcup install ghc 9.0.2 \
  && ghcup set ghc 9.0.2 \
  && ghcup install hls 1.8 \
  && ghcup set hls 1.8 \
  && ghcup install cabal 3.6 \
  && ghcup set cabal 3.6 \
  && cabal install hpack \
  && cabal install hspec-discover

# build and install souffle
RUN mkdir -p /tmp/souffle-src \
    && cd /tmp/souffle-src \
    && git clone https://github.com/souffle-lang/souffle.git \
    && cd souffle \
    && git checkout 2.3 \
    && cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
    && cmake --build build -j \
    && cmake --build build --target install \
    && cd /tmp && rm -rf /tmp/souffle-src

# add `split-file` and `FileCheck` utilities to `PATH`
RUN echo "export PATH=$PATH:/usr/lib/llvm-14/bin:/usr/bin" >> ~/.bashrc
RUN echo $PATH

# install `lit`
RUN pip install lit==14.0.6

WORKDIR /app/build
COPY . .

RUN cabal build

# Unit tests
RUN cabal run eclair-test

# Integration tests
RUN lit tests -v

# <= returns path to the Eclair compiler executable
RUN cabal list-bin eclair  

RUN echo -e "#!/bin/sh\nDATALOG_DIR=cbits/ `cabal list-bin eclair` $@" > /app/build/eclair.sh
RUN chmod u+x /app/build/eclair.sh
ENTRYPOINT ["/app/build/eclair.sh"]

VOLUME /code
