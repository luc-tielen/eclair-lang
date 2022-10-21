{
  description =
    "Eclair: An experimental and minimal Datalog that compiles to LLVM";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=master";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
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
  outputs = { self, np, fu, nu, ds, shs, llvm-cg, ... }@inputs:
    fu.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        ghcVersion = 902;
        llvmVersion = 14;

        mkVersion = v:
          "${v}.${np.lib.substring 0 8 self.lastModifiedDate}.${
            self.shortRev or "dirty"
          }";

        config = { };

        pkgs = import np {
          inherit system config;
          overlays = [ ds.overlay shs.overlay.${system} overlays.default ];
        };

        overlays.default = final: prev:
          let
            mkCabal2nix = nu.lib.mkCabal {
              inherit ghcVersion mkVersion;
              packages = final;
            };

            llvmPackages = final."llvmPackages_${toString llvmVersion}";

            haskellPackages =
              final.haskell.packages."ghc${toString ghcVersion}".override {
                overrides = hf: hp:
                  with final.haskell.lib; rec {
                    inherit (prev.haskellPackages)
                      souffle-haskell souffle-haskell-lint;

                    llvm-codegen = mkCabal2nix {
                      name = "llvm-codegen";
                      source = inputs.llvm-cg;
                      dependencies = { llvm-config = llvmPackages.llvm.dev; };
                      configureFlags = [ "--ghc-option=-optl=-lLLVM" ];
                      extraLibraries = [ llvmPackages.llvm.dev ];
                    };

                    algebraic-graphs = dontCheck (mkCabal2nix {
                      name = "algebraic-graphs";
                      source = inputs.alga;
                      doLibraryProfiling = enableLibraryProfiling;
                    });

                    dependent-hashmap =
                      unmarkBroken (dontCheck hp.dependent-hashmap);

                    diagnose =
                      hf.callCabal2nixWithOptions "diagnose" (inputs.diagnose)
                      "-fmegaparsec-compat" { };

                    eclair-lang = mkCabal2nix {
                      name = "eclair-lang";
                      source = self;
                      configureFlags = [ "--ghc-option=-optl=-lLLVM" ];
                      haskellPackages = hf;
                      extraLibraries = [ llvmPackages.llvm.dev ];
                      doStaticLibraries = enableStaticLibraries;
                      doSharedExecutables = disableSharedExecutables;
                      overrideAttrs = old: {
                        # /build/source/... is a virtual filesystem Nix uses
                        # when building Eclair, so by adding it to the path we
                        # can access the eclair executable during the tests.
                        checkPhase = ''
                          runHook preCheck
                          DATALOG_DIR="${self}/cbits/" SOUFFLE_BIN="${final.souffle}/bin/souffle" ./Setup test
                          PATH="${final.which}/bin:${final.souffle}/bin:$PWD/dist/build/eclair:${llvmPackages.clang}/bin/:${pkgs.nodejs}/bin:$PATH" ${final.lit}/bin/lit tests -v
                          runHook postCheck
                        '';
                      };
                    };
                  };
              };
          in {
            inherit haskellPackages llvmPackages;
            inherit (prev) souffle;
          };

        devShells.default = pkgs.devshell.mkShell {
          name = "Eclair";
          imports = [ (pkgs.devshell.importTOML ./devshell.toml) ];
          # packages are ordered lexicographically below
          packages = [
            pkgs.ghcid
            pkgs.lit
            pkgs.llvmPackages.bintools-unwrapped
            pkgs.llvmPackages.clang
            pkgs.llvmPackages.libllvm
            pkgs.llvmPackages.llvm.dev
            pkgs.nodejs
            pkgs.souffle
            (pkgs.haskellPackages.ghcWithPackages (p: [
              p.algebraic-graphs
              p.cabal-install
              p.ghc
              p.hsc2hs
              p.hpack
              p.haskell-language-server
              p.hlint
              p.hspec-discover
              p.llvm-codegen
              p.souffle-haskell
            ]))
          ];
          # Next line always sets DATALOG_DIR so souffle can find the datalog files in interpreted mode.
          env = [{
            name = "DATALOG_DIR";
            value = "${self}/cbits/";
          }];
        };

        packages.default = pkgs.haskellPackages.eclair-lang;

      in { inherit overlays packages devShells; });
}
