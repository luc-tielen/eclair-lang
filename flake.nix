{
  description =
    "eclair-lang: An experimental and minimal Datalog that compiles to LLVM";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
    shs.url = "github:luc-tielen/souffle-haskell?ref=master";
  };
  outputs = { self, np, fu, hls, shs }:
    with fu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        config = { };
        overlay = final: _:
          with final;
          with final.haskell.lib;
          with final.haskellPackages.extend (final: super:
            let
              lhsGit = fetchFromGitHub {
                owner = "luc-tielen";
                repo = "llvm-hs";
                rev = "69ae96c9eea8531c750c9d81f9813286ef5ced81";
                sha256 = "180ssgmi8f7j0wcbwj82rhn1375mgq9q8cirkrfw4yld1wz9wfpx";
              };
            in rec {
              inherit (shs.packages."${system}") souffle-haskell;

              llvm-config = llvmPackages_9.llvm;

              llvm-hs-pure = with final;
                addBuildTools (callCabal2nixWithOptions "llvm-hs-pure"
                  "${lhsGit}/llvm-hs-pure" "" { }) [ ];

              llvm-hs = with final;
                dontHaddock
                (callCabal2nixWithOptions "llvm-hs" "${lhsGit}/llvm-hs" "" {
                  inherit llvm-hs-pure;
                  llvm-config = llvmPackages_9.llvm;
                });

              llvm-hs-pretty = with final;
                dontCheck (overrideCabal
                  (callCabal2nixWithOptions "llvm-hs-pretty" (fetchFromGitHub {
                    owner = "luc-tielen";
                    repo = "llvm-hs-pretty";
                    rev = "990bb6981f6214d9c1bbf46fd9e9ce5596d3bf30";
                    sha256 =
                      "0kqb8n40zzv6jgcfi4wr40axz7r2q18rwzpq6qhpjnxwgg9qix0j";
                  }) "" { inherit llvm-hs llvm-hs-pure; })
                  (_: { patches = [ ./patches/1-llvm-hs-pretty.patch ]; }));

              llvm-hs-combinators = with final;
                callCabal2nixWithOptions "llvm-hs-combinators"
                (fetchFromGitHub {
                  owner = "luc-tielen";
                  repo = "llvm-hs-combinators";
                  rev = "6a5494d00d55dc2d988957588cf204731f27abc1";
                  sha256 =
                    "0l9l3h38mqr4m15a04awq6n0dmlz880zp18sxvi4iyr14qcfcyvw";
                }) "" { inherit llvm-hs-pure; };

              algebraic-graphs = with final;
                dontCheck (callCabal2nixWithOptions "algebraic-graphs"
                  (fetchFromGitHub {
                    owner = "snowleopard";
                    repo = "alga";
                    rev = "75de41a4323ab9e58ca49dbd78b77f307b189795";
                    sha256 =
                      "10jdy8hvjadnrrq2ch2sxcv9mk7l1q7p12w9d3bwhrgzfm3hb9sx";
                  }) "" { });
              haskell-stack-trace-plugin = with final;
                dontCheck (callHackage "haskell-stack-trace-plugin" "0.1.3.0" {
                  hspec = callHackage "hspec" "2.8.5" {
                    hspec-core = callHackage "hspec-core" "2.8.5" { };
                    hspec-discover = callHackage "hspec-discover" "2.8.5" { };
                  };
                });
            }); {
              eclair-lang =
                callCabal2nixWithOptions "eclair-lang" ./. "-fdebug" { };
            };
        overlays = [ overlay hls.overlay ] ++ shs.overlays."${system}";
      in with (import np { inherit system config overlays; });
      with np.lib; rec {
        inherit overlays;
        packages = flattenTree (recurseIntoAttrs { inherit eclair-lang; });
        defaultPackage = packages.eclair-lang;
        devShell = with haskellPackages;
          shellFor {
            packages = p: with p; [ souffle-haskell ];
            nativeBuildInputs = [ llvm-config ];
            buildInputs =
              [ hsc2hs haskell-language-server hpack ghc cabal-install ];
          };
      });
}
