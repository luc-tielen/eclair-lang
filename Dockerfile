FROM primordus/souffle-ubuntu:2.3
ARG LLVM_VERSION=17

SHELL [ "/bin/bash", "-c" ]

# install packages
RUN echo 'tzdata tzdata/Areas select Europe' | debconf-set-selections \
    && echo 'tzdata tzdata/Zones/Europe select Paris' | debconf-set-selections \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y \
       wget software-properties-common gnupg curl libffi-dev make \
       python3 python3-pip libgmp-dev \
    && curl -o - https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash \
    && source /root/.nvm/nvm.sh \
    && nvm install 18.1.0 \
    && echo "source /root/.ghcup/env" >> ~/.bashrc \
    # install llvm 17
    && mkdir -p /tmp/llvm-dir \
    && cd /tmp/llvm-dir \
    && wget https://apt.llvm.org/llvm.sh \
    && chmod +x llvm.sh \
    && ./llvm.sh $LLVM_VERSION \
    && cd /tmp \
    && rm -rf /tmp/llvm-dir \
    && cd /usr/bin \
    && ln -s /usr/lib/llvm-$LLVM_VERSION/bin/split-file \
    && ln -s /usr/lib/llvm-$LLVM_VERSION/bin/FileCheck \
    && ln -s clang-$LLVM_VERSION clang \
    && ln -s wasm-ld-$LLVM_VERSION wasm-ld \
    && cd - \
    && pip install lit==14.0.6 \
    # install ghcup, ghc-9.6.3 and cabal-3.10.1.0
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.6.3 BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh \
    && source /root/.ghcup/env \
    && cabal install cabal-fmt \
    && cabal install hspec-discover \
    && apt-get autoremove -y \
    && apt-get purge -y --auto-remove \
    && rm -rf /var/lib/apt/lists/*

VOLUME /code
WORKDIR /app/build
ENV DATALOG_DIR=/app/build/cbits

RUN echo -e '#!/bin/bash\nsource /root/.ghcup/env\nsource /root/.nvm/nvm.sh\nexec "$@"\n' > /app/build/entrypoint.sh \
    && chmod u+x /app/build/entrypoint.sh

# The entrypoint script sources ghcup setup script so we can easily call cabal etc.
ENTRYPOINT [ "/app/build/entrypoint.sh" ]

COPY . .

RUN source /root/.ghcup/env && make build \
    && echo -e '#!/bin/bash\nsource /root/.ghcup/env\n' > /usr/bin/eclair \
    && source /root/.ghcup/env \
    && echo -e "`cabal list-bin eclair` \"\$@\"" >> /usr/bin/eclair \
    && chmod u+x /usr/bin/eclair

# The default command to run, shows the help menu
CMD [ "eclair", "--help" ]
