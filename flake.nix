{
  description = "eclair-lang: souffle on LLVM";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
  };
  outputs = { self, np, fu, hls }:
    with fu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        config = { allowBroken = true; };
        overlay = final: _:
          with final;
          with final.haskell.lib;
          with final.haskellPackages.extend (final: super: {
            souffle-haskell = with super; dontCheck souffle-haskell;
            algebraic-graphs = with final;
              dontCheck (callCabal2nixWithOptions "algebraic-graphs"
                (fetchFromGitHub {
                  owner = "snowleopard";
                  repo = "alga";
                  rev = "75de41a4323ab9e58ca49dbd78b77f307b189795";
                  sha256 =
                    "10jdy8hvjadnrrq2ch2sxcv9mk7l1q7p12w9d3bwhrgzfm3hb9sx";
                }) "" { });
          }); {
            eclair-lang = dontCheck (callCabal2nix "eclair-lang" ./. {
              inherit algebraic-graphs souffle-haskell;
            });
          };
        overlays = [ overlay hls.overlay ];
      in with (import np { inherit system config overlays; });
      with np.lib; rec {
        inherit overlays;
        packages = flattenTree (recurseIntoAttrs { inherit eclair-lang; });
        defaultPackage = packages.eclair-lang;
        devShell = with haskellPackages;
          shellFor {
            packages = p:
              with p; [
                algebraic-graphs
                souffle-haskell
                llvm-hs
                llvm-hs-pure
                llvm-hs-pretty
              ];
            nativeBuildInputs = [ llvmPackages_9.llvm ];
            buildInputs =
              [ hsc2hs haskell-language-server hpack ghc cabal-install ];
          };
      });
}
