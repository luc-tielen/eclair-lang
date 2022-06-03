{
  description =
    "eclair-lang: An experimental and minimal Datalog that compiles to LLVM";
  inputs = {
    # np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
    shs.url =
      "github:luc-tielen/souffle-haskell?rev=c46d0677e4bc830df89ec1de2396c562eb9d86d3";
    llvm-cg.url =
      "github:luc-tielen/llvm-codegen?rev=fbce3c709e6238c84029a98a8115f437c21cb507";
    alga.url =
      "github:snowleopard/alga?rev=75de41a4323ab9e58ca49dbd78b77f307b189795";
    alga.flake = false;
  };
  outputs = { self, fu, ds, shs, llvm-cg, ... }@inputs:
    let
      # Use the same nixpkgs as souffle-haskell, to avoid weird issues with multiple versions of Haskell packages.
      np = shs.inputs.np;
    in
      with np.lib;
      with fu.lib;
      eachSystem [ "x86_64-linux" ] (system:
        let
          ghcVersion = "902";
          version = "${ghcVersion}.${substring 0 8 self.lastModifiedDate}.${
              self.shortRev or "dirty"
            }";
          config = {};
          overlay = final: _:
            let
              haskellPackages =
                final.haskell.packages."ghc${ghcVersion}".override {
                  overrides = hf: hp:
                    with final.haskell.lib; rec {
                      inherit (shs.packages."${system}") souffle-haskell;
                      inherit (llvm-cg.packages."${system}") llvm-codegen;

                      algebraic-graphs = with hf;
                        dontCheck
                        (callCabal2nix "algebraic-graphs" (inputs.alga) { });

                      dependent-hashmap = with hf;
                        unmarkBroken (dontCheck hp.dependent-hashmap);

                      eclair-lang = with hf;
                        (callCabal2nix "eclair-lang" ./. { }).overrideAttrs
                        (o: {
                          version = "${o.version}.${version}";
                          checkPhase = ''
                          runHook preCheck
                          DATALOG_DIR="${o.src}/cbits/" SOUFFLE_BIN="${pkgs.souffle}/bin/souffle" ./Setup test
                          runHook postCheck
                          '';
                        });
                    };
                };
            in { inherit haskellPackages; };
          pkgs = import np {
            inherit system config;
            overlays = [
              ds.overlay
              shs.overlay."${system}"
              llvm-cg.overlay."${system}"
              overlay
            ];
          };
        in with pkgs.lib; rec {
          inherit overlay;
          packages = { inherit (pkgs.haskellPackages) eclair-lang; };
          defaultPackage = packages.eclair-lang;
          devShell = pkgs.devshell.mkShell {
            name = "ECLAIR-LANG";
            imports = [ (pkgs.devshell.importTOML ./devshell.toml) ];
            packages = with pkgs;
              with haskellPackages; [
                souffle
                pkgs.llvmPackages_14.llvm.dev
                pkgs.ghcid
                (ghcWithPackages (p:
                  with p; [
                    algebraic-graphs
                    hspec-discover
                    llvm-codegen
                    souffle-haskell
                    ghc
                    cabal-install
                    hsc2hs
                    hpack
                    haskell-language-server
                  ]))
              ];
            # Next line always sets DATALOG_DIR so souffle can find the datalog files in interpreted mode.
            env = [{ name = "DATALOG_DIR"; value = "cbits/"; }];
          };
        });
}
