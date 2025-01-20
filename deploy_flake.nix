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

      # Define the declarative PostgreSQL container
      postgresImage = pkgs.dockerTools.buildImage {
        name = "postgresql-alpine";
        tag = "latest";
        fromImage = pkgs.dockerTools.pullImage {
          imageName = "postgres";
          imageTag = "15-alpine";
        };
        config = {
          Cmd = [ "postgres" ];
          Env = [
            "POSTGRES_USER=cheeblr_user"
            "POSTGRES_PASSWORD=securepassword"
            "POSTGRES_DB=cheeblr"
          ];
          Volumes = {
            "/var/lib/postgresql/data" = {};
          };
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

      # Define the full stack deployment with PostgreSQL, backend, and frontend
      deploy = pkgs.writeShellApplication {
        name = "deploy";
        runtimeInputs = [ postgresImage pkgs.docker ];
        text = ''
          # Start PostgreSQL container
          docker run -d \
            --name cheeblr-db \
            -e POSTGRES_USER=cheeblr_user \
            -e POSTGRES_PASSWORD=securepassword \
            -e POSTGRES_DB=cheeblr \
            -v cheeblr_data:/var/lib/postgresql/data \
            -p 5432:5432 \
            postgres:15-alpine

          # Start the backend
          ./result/bin/cheeblr-backend &

          # Start the frontend
          vite
        '';
      };

    in {
      packages.default = pkgs.haskell-nix.hix.project {
        src = ./backend;
        evalSystem = system;
        inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
      }.flake.packages.default;

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