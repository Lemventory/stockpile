{
  description = "cheeblr";

  inputs = {
    # IOG inputs
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?rev=145cc9e8bb4cf78fb7414a73326a35efd2262eff";
      flake = false;
    };

    purescript-overlay = {
      url = "github:harryprayiv/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
        
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, iohkNix, CHaP, iogx, purescript-overlay, ... }:
    {
      nixosModules = {
        postgresql = import ./nix/postgresql-service.nix;
        default = { ... }: {
          imports = [ self.nixosModules.postgresql ];
        };
      };
    } // flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system: let
      
      name = "cheeblr";
      lib = nixpkgs.lib;

      overlays = [
        iohkNix.overlays.crypto
        purescript-overlay.overlays.default
      ];
      
      pkgs = import nixpkgs {
        inherit system overlays;
      };

      # Shell apps
      postgresModule = import ./nix/postgres-utils.nix {
        inherit pkgs name;
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

      backup-project = pkgs.writeShellApplication {
        name = "backup-project";
        runtimeInputs = with pkgs; [ rsync ];
        text = ''
          rsync -va --delete --exclude-from='.gitignore' --exclude='.git/' ~/workdir/cheeblr/ ~/plutus/workspace/scdWs/cheeblr/
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

      testbedModule = import ./nix/testbed.nix {
        inherit pkgs name;
      };

    in {
      legacyPackages = pkgs;

      devShell = pkgs.mkShell {
        inherit name;
        
        nativeBuildInputs = with pkgs; [
          pkg-config
          postgresql
          zlib
          openssl.dev
          libiconv
          openssl
          lsof
          tmux  # Added for testbed
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
          hlint
          zlib
          pgcli
          pkg-config
          openssl.dev
          libiconv
          openssl
          
          postgresModule.pg-start
          postgresModule.pg-connect
          postgresModule.pg-stop
          postgresModule.pg-cleanup
          postgresModule.pg-backup    
          postgresModule.pg-restore    
          postgresModule.pg-rotate-credentials  
          postgresModule.pg-create-schema      
          postgresModule.pg-stats              
          

          # pgadmin4-desktopmode
          # dbeaver-bin
          # pgmanage
          pgadmin4

          # DevShell tools
          rsync
          tmux
          backup-project
          spago-watch
          vite
          dev
          code-workspace
          testbedModule.testbed
          testbedModule.startServices
          testbedModule.stopServices

        ] ++ (pkgs.lib.optionals (system == "aarch64-darwin")
          (with pkgs.darwin.apple_sdk.frameworks; [
            Cocoa
            CoreServices
          ]));
          shellHook = ''
            # Set up PostgreSQL environment
            export PGDATA="$HOME/.local/share/${name}/postgres"
            export PGPORT="5432"
            export PGUSER="$(whoami)"
            export PGPASSWORD="postgres"
            export PGDATABASE="${name}"
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