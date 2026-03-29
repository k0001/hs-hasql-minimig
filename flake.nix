{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=46db2e09e1d3f113a13c0d7b81e2f221c63b8ce9";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
    hasql-transaction.url = "github:nikita-volkov/hasql-transaction/1.2.2";
    hasql-transaction.flake = false;
    hasql.url = "github:nikita-volkov/hasql/1.10.3";
    hasql.flake = false;
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
              settings = {
                hasql-minimig = {
                  check = false;
                  haddock = true;
                  libraryProfiling = true;
                };
                postgresql-binary.check = false;
                hasql.check = false;
                hasql-transaction.check = false;
              };
              packages = {
                hasql-transaction.source = inputs.hasql-transaction;
                hasql.source = inputs.hasql;
                postgresql-binary.source = "0.15.0.1";
              };
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
                  CREATE USER user1 WITH ENCRYPTED PASSWORD 'user1';
                '';
                initialDatabases = [ { name = "db1"; } ];
                initialScript.after = ''
                  ALTER DATABASE db1 OWNER TO user1;
                '';
              };
              settings.processes.pgweb = {
                environment.PGWEB_DATABASE_URL = "postgres://user1:user1@127.0.0.1:32421/db1";
                command = pkgs.pgweb;
                depends_on."pg1".condition = "process_healthy";
              };
            };
            apps.default = self'.packages.env-services;
            packages.default = self'.packages.ghc9122-hasql-minimig;
            packages.doc = self'.packages.ghc9122-hasql-minimig.doc;
            devShells.default = self'.devShells.ghc9122;
          };
      }
    );
}
