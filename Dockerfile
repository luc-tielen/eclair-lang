FROM ubuntu:20.04

# install packages
RUN echo 'tzdata tzdata/Areas select Europe' | debconf-set-selections \
    && echo 'tzdata tzdata/Zones/Europe select Paris' | debconf-set-selections
RUN apt-get update \
    && apt-get autoremove -y \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y \
       lsb-release wget software-properties-common gnupg \
       nodejs curl bison build-essential clang cmake \
       doxygen flex g++ git libffi-dev libncurses5-dev \
       libsqlite3-dev make mcpp python3 python3-pip sqlite zlib1g-dev libgmp-dev \
    && rm -rf /var/lib/apt/lists/* \
    && echo "source /root/.ghcup/env" >> ~/.bashrc

# build and install souffle
RUN mkdir -p /tmp/souffle-src \
    && cd /tmp/souffle-src \
    && git clone https://github.com/souffle-lang/souffle.git \
    && cd souffle \
    && git checkout 2.3 \
    && cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
    && cmake --build build -j \
    && cmake --build build --target install \
    && cd /tmp && rm -rf /tmp/souffle-src \
    # add `split-file` and `FileCheck` utilities to `PATH`
    && echo "export PATH=$PATH:/usr/lib/llvm-14/bin:/usr/bin" >> ~/.bashrc

# install llvm 14
RUN mkdir -p /tmp/llvm-dir \
    && cd /tmp/llvm-dir \
    && wget https://apt.llvm.org/llvm.sh \
    && chmod +x llvm.sh \
    && ./llvm.sh 14 \
    && cd /tmp \
    && rm -rf /tmp/llvm-dir \
    && pip install lit==14.0.6

# install ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# install and set ghc
SHELL [ "/bin/bash", "-c", "source /root/.ghcup/env" ]
RUN ghcup install ghc 9.0.2 \
    && ghcup set ghc 9.0.2 \
    && ghcup install cabal --force 3.6.2.0 \
    && ghcup set cabal 3.6.2.0 \
    && cabal install hspec-discover

VOLUME /code
WORKDIR /app/build
ENV DATALOG_DIR=/app/build/cbits
COPY . .

RUN make build \
    && echo -e "#!/bin/bash\nset -e\nexec \"$@\"" > /app/build/entrypoint.sh \
    && chmod u+x /app/build/entrypoint.sh
#ENTRYPOINT [ "/app/build/entrypoint.sh" ]

# The default command to run, shows the help menu
CMD [ "`cabal list-bin eclair`", "--help" ]
