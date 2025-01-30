{
  description = "cheeblr";

  inputs = {
    # IOG inputs
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      # inputs.haskell-nix.follows = "haskellNix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    # haskellNix = {
    #   url = "github:input-output-hk/haskell.nix/1c329acdaac3d5a600bcaa86b1806414ccd48db6";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.hackage.follows = "hackage";
    # };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?rev=35d5d7f7e7cfed87901623262ceea848239fa7f8";
      flake = false;
    };

    # PureScript inputs
    purescript-overlay = {
      url = "github:harryprayiv/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
        
    # Utils
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, iohkNix, CHaP, iogx, purescript-overlay, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system: let
      name = "cheeblr";

      postgresUtils = import ./sqlShell.nix { inherit pkgs; };      
      
      overlays = [
        # haskellNix.overlay
        iohkNix.overlays.crypto
        purescript-overlay.overlays.default
      ];
      
      pkgs = import nixpkgs {
        inherit system overlays;
        # inherit (haskellNix) config;
        # config.allowUnfree = true;
      };

      vite = pkgs.writeShellApplication {
        name = "vite";
        runtimeInputs = with pkgs; [ nodejs-slim ];
        text = ''
          export CHEEBLR_BASE_PATH="${self}"
          npx vite --open
        '';
      };

      concurrent = pkgs.writeShellApplication {
        name = "concurrent";
        runtimeInputs = with pkgs; [ concurrently ];
        text = ''
          concurrently\
            --color "auto"\
            --prefix "[{command}]"\
            --handle-input\
            --restart-tries 10\
            "$@"
        '';
      };

      spago-watch = pkgs.writeShellApplication {
        name = "spago-watch";
        runtimeInputs = with pkgs; [ entr spago-unstable ];
        text = ''find {src,test} | entr -s "spago $*" '';
      };

      code-workspace = pkgs.writeShellApplication {
        name = "code-workspace";
        runtimeInputs = with pkgs; [ vscodium ];
        text = ''
          codium cheeblr.code-workspace
        '';
      };

      dev = pkgs.writeShellApplication {
        name = "dev";
        runtimeInputs = with pkgs; [
          nodejs-slim
          spago-watch
          vite
          concurrent
        ];
        text = ''
          concurrent "spago-watch build" vite
        '';
      };
      
      # hixProject = pkgs.haskell-nix.hix.project {
      #   src = ./backend;
      #   evalSystem = system;
      #   inputMap = {"https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;};
      #   projectFileName = "nix/hix.nix";
      #   modules = [{
      #     packages = {
      #       cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf];
      #       cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf pkgs.secp256k1];
              
      #       "${name}".components.library = {
      #         buildable = true;
      #       };
              
      #       "postgresql-libpq" = {
      #         flags.use-pkg-config = false;
      #         package = {
      #           buildType = "Simple";
      #           buildable = true;
      #           cleanHpack = false;
      #         };
      #         components.library = {
      #           buildable = true;
      #           planned = true;
      #           preBuild = ''
      #             export PATH="${pkgs.postgresql}/bin:$PATH"
      #             export PG_CONFIG="${pkgs.postgresql4}/bin/pg_config"
      #           '';
      #           buildInputs = [ pkgs.postgresql ];
      #           librarySystemDepends = [ pkgs.postgresql ];
      #           buildToolDepends = [ pkgs.pkg-config pkgs.postgresql ];
      #           configureFlags = [ "--flags=-use-pkg-config" ];
      #         };
      #       };
      #     };
      #   }];
      # };
      
      # hixFlake = hixProject.flake {};
    in {
      # packages = {
      #   default = hixFlake.packages."cheeblr-backend:exe:cheeblr-backend" or hixFlake.packages.default;
      # } // hixFlake.packages;

      # apps = hixFlake.apps;
      # checks = hixFlake.checks;
      legacyPackages = pkgs;

      devShell = pkgs.mkShell {
        inherit name;
        # inputsFrom = [hixFlake.devShell];
        
        nativeBuildInputs = with pkgs; [
          pkg-config
          postgresql
          zlib
          openssl.dev
          libiconv
          openssl
        ];
        buildInputs = with pkgs; [
          # Front End tools
          esbuild
          nodejs_20
          nixpkgs-fmt
          purs
          purs-tidy
          purs-backend-es
          purescript-language-server
          spago-unstable
      
          # Back End tools
          cabal-install
          ghc
          haskellPackages.fourmolu
          haskell-language-server
          zlib
          pgcli
          postgresql
          pkg-config
          openssl.dev
          libiconv
          openssl
          
          # Custom DevShell tools
          spago-watch
          vite
          dev
          code-workspace
          
          postgresUtils.pg-start
          postgresUtils.pg-stop
          postgresUtils.pg-reset
          postgresUtils.pg-setup
          postgresUtils.pg-console

        ] ++ (pkgs.lib.optionals (system == "aarch64-darwin")
          (with pkgs.darwin.apple_sdk.frameworks; [
            Cocoa
            CoreServices
          ]));

        shellHook = ''
          if ! test -d .nix-shell; then
            mkdir .nix-shell
          fi

          export NIX_SHELL_DIR=$PWD/.nix-shell
          # Put the PostgreSQL databases in the project directory.
          export PGDATA=$NIX_SHELL_DIR/db

          # Ensure pg_config is in PATH
          export PATH="${pkgs.postgresql}/bin:$PATH"
          export PG_CONFIG="${pkgs.postgresql}/bin/pg_config"
          # export PKG_CONFIG_PATH="${pkgs.postgresql}/lib/pkgconfig:${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
          export LIBRARY_PATH="${pkgs.postgresql}/lib:$LIBRARY_PATH"
          export LD_LIBRARY_PATH="${pkgs.postgresql}/lib:$LD_LIBRARY_PATH"
          export CPATH="${pkgs.postgresql}/include:$CPATH"
          
          echo "Development environment:"
          echo "  PG_CONFIG=$PG_CONFIG"
          echo "  PATH includes pg_config: $(which pg_config)"
          
          echo "Available commands: spago-watch, vite, dev, code-workspace, pg-start, pg-stop, pg-reset, pg-setup, pg-console"
          echo ""
          echo "Version Info:"
          echo ""
          echo "$(ghc --version)"
          echo "$(cabal --version)"
          echo "Purescript: version $(purs --version)"
          echo "Spago: version $(spago --version)"
          echo "postgresql: version $(${pkgs.postgresql.dev}/bin/pg_config --version)"
        '';
      };
    });

  nixConfig = {
    extra-experimental-features = ["nix-command flakes" "ca-derivations"];
    allow-import-from-derivation = "true";
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
  };
}