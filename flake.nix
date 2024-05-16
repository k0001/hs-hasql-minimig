{
  description = "Haskell 'hasql-minimig' library";

  inputs = {
    flakety.url = "github:k0001/flakety";
    nixpkgs.follows = "flakety/nixpkgs";
    flake-parts.follows = "flakety/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.flakety.overlays.default
        (final: prev:
          let
            hsLib = prev.haskell.lib;
            hsClean = drv:
              hsLib.overrideCabal drv
              (old: { src = prev.lib.sources.cleanSource old.src; });
          in {
            haskell = prev.haskell // {
              packageOverrides = prev.lib.composeExtensions
                (prev.haskell.packageOverrides or (_: _: { })) (hfinal: hprev:
                  prev.lib.optionalAttrs
                  (prev.lib.versionAtLeast hprev.ghc.version "9.6") {
                    hasql-minimig = hsLib.doBenchmark
                      (hfinal.callPackage ./hasql-minimig { });
                  });
            };
          })
      ];
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          hasql-minimig__ghc98 = pkgs.haskell.packages.ghc98.hasql-minimig;
          hasql-minimig__ghc98__sdist =
            pkgs.haskell.packages.ghc98.cabalSdist { src = ./hasql-minimig; };
          hasql-minimig__ghc98__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.hasql-minimig__ghc98;
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.hasql-minimig__ghc98
              config.packages.hasql-minimig__ghc98.doc
              config.packages.hasql-minimig__ghc98__sdist
              config.packages.hasql-minimig__ghc98__sdistDoc
              config.devShells.ghc98
            ];
          };
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.hasql-minimig ];
              doBenchmark = true;
              withHoogle = true;
              nativeBuildInputs = [
                pkgs.cabal-install
                pkgs.cabal2nix
                pkgs.ghcid
                pkgs.ephemeralpg
              ];
            };
        in {
          default = config.devShells.ghc98;
          ghc98 = mkShellFor pkgs.haskell.packages.ghc98;
        };
      };
    };
}
