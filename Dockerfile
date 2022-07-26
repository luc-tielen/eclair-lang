FROM nixos/nix:2.9.2

WORKDIR /app/build
RUN nix profile install nixpkgs#cachix --extra-experimental-features nix-command --extra-experimental-features flakes && cachix use cachix && cachix use luctielen

RUN echo -e '#!/bin/sh\nDATALOG_DIR=cbits/ PATH="$(find /nix/store -type f | grep souffle$ | xargs dirname):$PATH" ./result/bin/eclair $@' > ./eclair.sh
ENTRYPOINT ["./eclair.sh"]

COPY . .
RUN nix build --extra-experimental-features nix-command --extra-experimental-features flakes
RUN chmod u+x ./eclair.sh

VOLUME /code
