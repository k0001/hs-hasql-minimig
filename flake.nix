{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      let
        # mapListToAttrs f [a b] = {a = f a; b = f b;}
        mapListToAttrs =
          f: xs:
          builtins.listToAttrs (
            builtins.map (x: {
              name = x;
              value = f x;
            }) xs
          );
        ghcVersions = [
          "ghc984"
          "ghc9102"
          "ghc9122"
        ];
      in
      {
        systems = nixpkgs.lib.systems.flakeExposed;
        imports = [
          inputs.haskell-flake.flakeModule
          inputs.process-compose-flake.flakeModule
        ];
        flake.haskellFlakeProjectModules = mapListToAttrs (
          ghc:
          (
            { pkgs, lib, ... }:
            withSystem pkgs.system (
              { config, ... }: config.haskellProjects.${ghc}.defaults.projectModules.output
            )
          )
        ) ghcVersions;
        perSystem =
          {
            self',
            pkgs,
            config,
            ...
          }:
          {
            haskellProjects = mapListToAttrs (ghc: {
              basePackages = pkgs.haskell.packages.${ghc};
              settings.hasql-minimig.check = false;
              settings.hasql-minimig.haddock = true;
              packages.brick.source = "2.9";
              autoWire = [
                "packages"
                "checks"
                "devShells"
              ];
              devShell = {
                tools = hp: {
                  inherit (pkgs) cabal2nix;
                  inherit (self'.packages) env-services;
                };
                mkShellArgs = {
                  inputsFrom = [
                    config.process-compose.env-services.services.outputs.devShell
                  ];
                  HASQL_MINIMIG_TEST_CONNSTRING =
                    config.process-compose.env-services.services.postgres.pg1.connectionURI
                      { dbName = "db1"; };
                };
              };
            }) ghcVersions;
            process-compose.env-services = p: {
              imports = [ inputs.services-flake.processComposeModules.default ];
              services.postgres.pg1 = {
                enable = true;
                port = 32421;
                initialScript.before = ''
                  CREATE USER user1 WITH PASSWORD 'user1';
                '';
                initialDatabases = [ { name = "db1"; } ];
                initialScript.after = ''
                  ALTER DATABASE db1 OWNER TO user1;
                '';
              };
              settings.processes.pgweb = {
                environment.PGWEB_DATABASE_URL = p.config.services.postgres.pg1.connectionURI { dbName = "db1"; };
                command = pkgs.pgweb;
                depends_on."pg1".condition = "process_healthy";
              };
            };
            packages.default = self'.packages.env-services;
            devShells.default = self'.devShells.ghc9122;
          };
      }
    );
}
