{
  description = "cheeblr";

  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskellNix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage";
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages";
      flake = false;
    };

    purescript-overlay = {
      url = "github:harryprayiv/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP, iogx, purescript-overlay, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      name = "cheeblr";

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.crypto
        purescript-overlay.overlays.default
        (final: prev: {
          cheeblr-backend = final.haskell-nix.project' {
            src = ./backend;
            compiler-nix-name = "ghc928";
            shell.tools = {
              cabal = "latest";
              haskell-language-server = "latest";
            };
            shell.buildInputs = with pkgs; [
              zlib
              cabal-install
              haskell-language-server
            ];
          };
        })
      ];

      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };

      # PostgreSQL OCI container
      postgresContainer = pkgs.ociTools.buildContainer {
        args = [
          (pkgs.writeShellScriptBin "run-postgres" ''
            exec postgres -D /var/lib/postgresql/data \
              -c listen_addresses='*' \
              -c max_connections=50
          '').outPath
        ];
        mounts."/var/lib/postgresql/data" = {
          source = "/data/postgres";
        };
        env = {
          POSTGRES_USER = "cheeblr_user";
          POSTGRES_PASSWORD = "securepassword";
          POSTGRES_DB = "cheeblr";
        };
      };

      # Backend OCI container
      backendContainer = pkgs.ociTools.buildContainer {
        args = [ "${pkgs.haskellPackages.cheeblr-backend}/bin/cheeblr-backend" ];
        mounts."/config" = {
          source = "/etc/cheeblr/config";
        };
        env = {
          DB_HOST = "127.0.0.1";
          DB_USER = "cheeblr_user";
          DB_PASSWORD = "securepassword";
          DB_NAME = "cheeblr";
        };
      };

      # PureScript development tools
      vite = pkgs.writeShellApplication {
        name = "vite";
        runtimeInputs = with pkgs; [ nodejs-slim ];
        text = ''
          export CHEEBLR_BASE_PATH="${self}"
          npx vite --open
        '';
      };

      spago-watch = pkgs.writeShellApplication {
        name = "spago-watch";
        runtimeInputs = with pkgs; [ entr spago-unstable ];
        text = ''find {src,test} | entr -s "spago $*" '';
      };

      dev = pkgs.writeShellApplication {
        name = "dev";
        runtimeInputs = with pkgs; [
          nodejs-slim
          spago-watch
          vite
        ];
        text = ''
          concurrent "spago-watch build" vite
        '';
      };

      # Deployment script
      deploy = pkgs.writeShellApplication {
        name = "deploy";
        runtimeInputs = [ postgresContainer backendContainer pkgs.runc ];
        text = ''
          # Start PostgreSQL
          runc run postgres-container

          # Start the backend
          runc run backend-container
        '';
      };

    in {
      packages.default = backendContainer;
      devShell = pkgs.mkShell {
        buildInputs = [
          pkgs.esbuild
          pkgs.nodejs_20
          pkgs.purs
          pkgs.spago-unstable
          pkgs.cabal-install
          pkgs.haskell-language-server
          vite
          dev
          spago-watch
        ];
        shellHook = ''
          echo "Welcome to the Cheeblr dev environment!"
          echo "Available commands: spago-watch vite dev deploy"
        '';
      };

      apps.deploy = deploy;
    });
}