{
  description =
    "eclair-lang: An experimental and minimal Datalog that compiles to LLVM";
  inputs = {
    # np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
    nf.url = "github:numtide/nix-filter?ref=master";
    nu.url = "github:smunix/nix-utils?ref=main";
    hls.url = "github:haskell/haskell-language-server?ref=master";
    shs.url =
      "github:luc-tielen/souffle-haskell?rev=c46d0677e4bc830df89ec1de2396c562eb9d86d3";
    llvm-cg.url = "github:luc-tielen/llvm-codegen";
    llvm-cg.flake = false;
    alga.url =
      "github:snowleopard/alga?rev=75de41a4323ab9e58ca49dbd78b77f307b189795";
    alga.flake = false;
    diagnose.url =
      "github:luc-tielen/diagnose?rev=d58752f062c105ec0f8831357f3c688965e13add";
    diagnose.flake = false;
  };
  outputs = { self, fu, nf, nu, ds, shs, llvm-cg, ... }@inputs:
    let
      # Use the same nixpkgs as souffle-haskell, to avoid weird issues with multiple versions of Haskell packages.
      np = shs.inputs.np;
    in with np.lib;
    with fu.lib;
    with nu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        ghcVersion = "902";
        llvmVersion = 14;
        version = "${ghcVersion}.${substring 0 8 self.lastModifiedDate}.${
            self.shortRev or "dirty"
          }";
        config = { };
        overlay = final: _:
          let
            llvmPackages = rec {
              llvmPkgs = final."llvmPackages_${toString llvmVersion}";
              inherit (llvmPkgs) llvm libllvm bintools-unwrapped;
            };
            haskellPackages =
              final.haskell.packages."ghc${ghcVersion}".override {
                overrides = hf: hp:
                  with final.haskell.lib; rec {
                    inherit (shs.packages."${system}") souffle-haskell;

                    llvm-codegen = addExtraLibraries ( haskellSharedLibExe final
                        (appendConfigureFlags
                          ((hf.callCabal2nix "llvm-codegen" (with nf.lib;
                            filter {
                              root = inputs.llvm-cg;
                              exclude = [ ("Setup.hs") ];
                            }) {
                              llvm-config = llvmPackages.llvm;
                            }).overrideAttrs
                            (old: { version = "${old.version}-${version}"; }))
                          [ "--ghc-option=-optl=-lLLVM" ]))
                      [ llvmPackages.llvm.dev ];

                    algebraic-graphs = with hf;
                      dontCheck
                      (callCabal2nix "algebraic-graphs" (inputs.alga) { });

                    dependent-hashmap = with hf;
                      unmarkBroken (dontCheck hp.dependent-hashmap);

                    diagnose =
                      hf.callCabal2nixWithOptions "diagnose" (inputs.diagnose)
                      "-fmegaparsec-compat" { };

                    eclair-lang = with hf;
                      with final.haskell.lib;
                      addExtraLibraries (
                          haskellSharedLibExe final (appendConfigureFlags
                            ((callCabal2nix "eclair-lang" ./. { }).overrideAttrs
                              (o: {
                                version = "${o.version}.${version}";
                                checkPhase = ''
                                  runHook preCheck
                                  DATALOG_DIR="${o.src}/cbits/" SOUFFLE_BIN="${pkgs.souffle}/bin/souffle" ./Setup test
                                  runHook postCheck
                                '';
                              })) [ "--ghc-option=-optl=-lLLVM" ]))
                      [ llvmPackages.llvm.dev ];
                  };
              };
          in { inherit haskellPackages llvmPackages; };
        pkgs = import np {
          inherit system config;
          overlays = [
            ds.overlay
            shs.overlay."${system}"
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
              pkgs.ghcid
              llvmPackages.libllvm
              llvmPackages.llvm.dev
              llvmPackages.bintools-unwrapped
              (ghcWithPackages (p:
                with p; [
                  algebraic-graphs
                  cabal-install
                  ghc
                  hsc2hs
                  hpack
                  haskell-language-server
                  hlint
                  hspec-discover
                  llvm-codegen
                  souffle-haskell
                ]))
            ];
          # Next line always sets DATALOG_DIR so souffle can find the datalog files in interpreted mode.
          env = [{
            name = "DATALOG_DIR";
            value = "cbits/";
          }];
        };
      });
}
