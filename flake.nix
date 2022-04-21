{
  description =
    "eclair-lang: An experimental and minimal Datalog that compiles to LLVM";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
    shs.url =
      "github:luc-tielen/souffle-haskell?rev=7bfa29f52f2056459d160e795d2e7b8f36af801a";
    llvm-hs.url =
      "github:luc-tielen/llvm-hs?rev=69ae96c9eea8531c750c9d81f9813286ef5ced81";
    llvm-hs.flake = false;
    llvm-hs-pretty.url =
      "github:luc-tielen/llvm-hs-pretty?rev=990bb6981f6214d9c1bbf46fd9e9ce5596d3bf30";
    llvm-hs-pretty.flake = false;
    llvm-hs-combinators.url =
      "github:luc-tielen/llvm-hs-combinators?rev=6a5494d00d55dc2d988957588cf204731f27abc1";
    llvm-hs-combinators.flake = false;
    alga.url =
      "github:snowleopard/alga?rev=75de41a4323ab9e58ca49dbd78b77f307b189795";
    alga.flake = false;
  };
  outputs = { self, np, fu, ds, shs, ... }@inputs:
    with np.lib;
    with fu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        ghcVersion = "8107";
        version = "${ghcVersion}.${substring 0 8 self.lastModifiedDate}.${
            self.shortRev or "dirty"
          }";
        config = { };
        overlay = final: _:
          let
            haskellPackages =
              final.haskell.packages."ghc${ghcVersion}".override {
                overrides = hf: hp:
                  with final.haskell.lib; {
                    inherit (shs.packages."${system}") souffle-haskell;

                    llvm-config = final.llvmPackages_9.llvm;

                    llvm-hs-pure = with hf;
                      (callCabal2nix "llvm-hs-pure"
                        "${inputs.llvm-hs}/llvm-hs-pure" { });

                    llvm-hs = with hf;
                      dontHaddock
                      (callCabal2nix "llvm-hs" "${inputs.llvm-hs}/llvm-hs" { });

                    llvm-hs-pretty = with hf;
                      dontCheck (doJailbreak
                        (callCabal2nix "llvm-hs-pretty" (inputs.llvm-hs-pretty)
                          { }));

                    llvm-hs-combinators = with hf;
                      callCabal2nix "llvm-hs-combinators"
                      (inputs.llvm-hs-combinators) { };

                    algebraic-graphs = with hf;
                      dontCheck
                      (callCabal2nix "algebraic-graphs" (inputs.alga) { });

                    relude = hf.relude_1_0_0_1;

                    eclair-lang = with hf;
                      (callCabal2nix "eclair-lang" ./. { }).overrideAttrs
                      (o: { version = "${o.version}.${version}"; });
                  };
              };
          in { inherit haskellPackages; };

        pkgs = import np {
          inherit system config;
          overlays = [ overlay ds.overlay ] ++ shs.overlays."${system}";
        };
      in with pkgs.lib; rec {
        inherit overlay;
        packages = { inherit (pkgs.haskellPackages) eclair-lang; };
        defaultPackage = packages.eclair-lang;
        devShell = pkgs.devshell.mkShell {
          name = "ECLAIR-LANG";
          imports = [ (pkgs.devshell.importTOML ./devshell.toml) ];
          packages = with pkgs;
            with haskellPackages;
            [
              (ghcWithPackages (p:
                with p; [
                  eclair-lang
                  algebraic-graphs
                  llvm-hs
                  llvm-hs-pure
                  llvm-hs-pretty
                  llvm-hs-combinators
                  llvm-config
                  souffle-haskell
                  ghc
                  cabal-install
                  hsc2hs
                  hpack
                  pkgs.ghcid
                  haskell-language-server
                ]))
            ];
        };
      });
}
