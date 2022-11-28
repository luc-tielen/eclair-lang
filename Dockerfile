FROM primordus/souffle-ubuntu:2.3
ARG LLVM_VERSION=14

SHELL [ "/bin/bash", "-c" ]

# install packages
RUN echo 'tzdata tzdata/Areas select Europe' | debconf-set-selections \
    && echo 'tzdata tzdata/Zones/Europe select Paris' | debconf-set-selections
RUN apt-get update \
    && apt-get autoremove -y \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y \
       wget software-properties-common gnupg curl libffi-dev make \
       python3 python3-pip libgmp-dev \
    && rm -rf /var/lib/apt/lists/* \
    && curl -o - https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash \
    && source /root/.nvm/nvm.sh \
    && nvm install 18.1.0 \
    && echo "source /root/.ghcup/env" >> ~/.bashrc \
    # install llvm 14
    && mkdir -p /tmp/llvm-dir \
    && cd /tmp/llvm-dir \
    && wget https://apt.llvm.org/llvm.sh \
    && chmod +x llvm.sh \
    && ./llvm.sh $LLVM_VERSION \
    && cd /tmp \
    && rm -rf /tmp/llvm-dir \
    && cd /usr/bin \
    && ln -s /usr/lib/llvm-14/bin/split-file \
    && ln -s /usr/lib/llvm-14/bin/FileCheck \
    && ln -s clang-14 clang \
    && ln -s wasm-ld-14 wasm-ld \
    && cd - \
    && pip install lit==14.0.6 \
    # install ghcup
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# install and set ghc
RUN source /root/.ghcup/env \
    && ghcup install ghc --force 9.2.4 && ghcup set ghc 9.2.4 \
    && ghcup install cabal --force 3.6.2.0 && ghcup set cabal 3.6.2.0 \
    && cabal install hspec-discover

VOLUME /code
WORKDIR /app/build
ENV DATALOG_DIR=/app/build/cbits

RUN echo -e '#!/bin/bash\nsource /root/.ghcup/env\nsource /root/.nvm/nvm.sh\nexec "$@"\n' > /app/build/entrypoint.sh \
    && chmod u+x /app/build/entrypoint.sh

# The entrypoint script sources ghcup setup script so we can easily call cabal etc.
ENTRYPOINT [ "/app/build/entrypoint.sh" ]

COPY . .

RUN source /root/.ghcup/env && make build
RUN echo -e '#!/bin/bash\nsource /root/.ghcup/env\n' > /usr/bin/eclair \
    && source /root/.ghcup/env \
    && echo -e "`cabal list-bin eclair` \"\$@\"" >> /usr/bin/eclair \
    && chmod u+x /usr/bin/eclair

# The default command to run, shows the help menu
CMD [ "eclair", "--help" ]
