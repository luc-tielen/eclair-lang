# syntax=docker/dockerfile:1.4
FROM nixos/nix:2.9.2 AS builder
WORKDIR /app/build
RUN nix profile install nixpkgs#cachix --extra-experimental-features nix-command --extra-experimental-features flakes && cachix use cachix && cachix use luctielen

# TODO generate this using Nix to avoid heredoc syntax (this also avoids the export command)
COPY <<EOF ./eclair.sh
#!/bin/sh
DATALOG_DIR=cbits/ PATH="\$(find /nix/store -type f | grep souffle$ | xargs dirname):\$PATH" ./result/bin/eclair \$@
EOF

ENTRYPOINT ["./eclair.sh"]

COPY . .
RUN nix build --extra-experimental-features nix-command --extra-experimental-features flakes
RUN chmod u+x ./eclair.sh

VOLUME /code
