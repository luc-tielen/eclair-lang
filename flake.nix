{
  description =
    "eclair-lang: An experimental and minimal Datalog that compiles to LLVM";

  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.souffle-haskell.url =
    "github:smunix/souffle-haskell?ref=fix.ghc-multi";
  inputs.llvm-codegen.url = "github:smunix/llvm-codegen?ref=fix.ghc-multi";
  inputs.algebraic-graphs.url =
    "github:snowleopard/alga?rev=75de41a4323ab9e58ca49dbd78b77f307b189795";
  inputs.algebraic-graphs.flake = false;

  outputs = { self, flake-utils, nix-filter, devshell, nixpkgs, ... }@inputs:
    with nixpkgs.lib;
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        rmDot = replaceStrings [ "." ] [ "" ];
        supportedGHCs = [ "default" "902" "922" ];
        config = { };
        overlays.devshell = devshell.overlay;
        overlays.default = f: p:
          let
            ghcVersion = "ghc${rmDot p.haskellPackages.ghc.version}";

            mkHaskellPackages = hspkgs:
              (hspkgs.override (old: { })).extend (hf: hp:
                with f.haskell.lib;
                composeExtensions (hf: hp: {
                  eclair-lang = disableLibraryProfiling
                    ((hf.callCabal2nix "eclair-lang" (with nix-filter.lib;
                      filter {
                        root = self;
                        exclude = [ (matchExt "cabal") ];
                      }) { }).overrideAttrs (old: {
                        version = "${rmDot hp.ghc.version}-${old.version}-${
                            substring 0 8 self.lastModifiedDate
                          }.${self.shortRev or "dirty"}";
                      }));
                }) (hf: hp:
                  with f.haskell.lib; {
                    algebraic-graphs = dontCheck
                      (hf.callCabal2nix "algebraic-graphs"
                        (inputs.algebraic-graphs) { });

                    dependent-hashmap =
                      unmarkBroken (dontCheck hp.dependent-hashmap);

                    souffle-haskell =
                      inputs.souffle-haskell.packages.${system}."souffle-haskell-${
                        rmDot hp.ghc.version
                      }";

                    llvm-codegen =
                      inputs.llvm-codegen.packages.${system}."llvm-codegen-${
                        rmDot hp.ghc.version
                      }";
                  }) hf hp);

            # all haskellPackages
            allHaskellPackages = let
              cases = listToAttrs (map (n: {
                name = "${n}";
                value = mkHaskellPackages
                  f.haskell.packages."${if n == "default" then
                    "${ghcVersion}"
                  else
                    "ghc${n}"}";
              }) supportedGHCs);
            in cases;

            # all packages
            allPackages = listToAttrs (map (n: {
              name = if n == "default" then n else "eclair-lang-${n}";
              value = allHaskellPackages."${n}".eclair-lang;
            }) supportedGHCs);

            # make dev shell
            mkDevShell = g:
              p.devshell.mkShell {
                name = "eclair-lang-${
                    if g == "default" then "${ghcVersion}" else g
                  }-${substring 0 8 self.lastModifiedDate}.${
                    self.shortRev or "dirty"
                  }";
                packages = with f;
                  with f.allHaskellPackages."${g}"; [
                    ghcid
                    (ghcWithPackages (hp:
                      with hp; [
                        eclair-lang
                        cabal-install
                        ghc
                        haskell-language-server
                        hpack
                        hspec-discover
                        hsc2hs
                        llvm-codegen
                        souffle-haskell
                      ]))
                  ];
              };

            # all packages
            allDevShells = listToAttrs (map (n: {
              name = "${n}";
              value = mkDevShell n;
            }) supportedGHCs);
          in {
            haskellPackages = allHaskellPackages.default;
            inherit allHaskellPackages allDevShells allPackages;
          };

        pkgs = import nixpkgs {
          inherit system config;
          overlays = [ overlays.devshell overlays.default ];
        };

      in with pkgs.lib; rec {
        inherit overlays;
        packages = flattenTree (pkgs.recurseIntoAttrs pkgs.allPackages);
        devShells = flattenTree (pkgs.recurseIntoAttrs pkgs.allDevShells);
      });
}
