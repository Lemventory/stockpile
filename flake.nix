{
  description = "cheeblr";

  inputs = {
    # IOG inputs
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
      url = "github:input-output-hk/haskell.nix/1c329acdaac3d5a600bcaa86b1806414ccd48db6";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage";
    };

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

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP, iogx, purescript-overlay, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system: let
      name = "cheeblr";
      
      overlays = [
        haskellNix.overlay
        iohkNix.overlays.crypto
        purescript-overlay.overlays.default
        (final: prev: {
          cheeblr-backend = final.haskell-nix.project' {
            # Point to the backend directory
            src = ./backend;
            compiler-nix-name = "ghc928";
            shell.tools = {
              cabal = "latest";
              haskell-language-server = "latest";
            };
            shell.buildInputs = with pkgs; [
              zlib
              cabal-install 
            ];
          };
        })
      ];
      
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };

      # PureScript shell apps
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
      
      # Haskell project setup
      hixProject = pkgs.haskell-nix.hix.project {
        src = ./backend;
        evalSystem = system;
        inputMap = {"https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;};
        projectFileName = "nix/hix.nix";
        modules = [
          (_: {
            packages.cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf];
            packages.cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [pkgs.libsodium-vrf pkgs.secp256k1];
          })
        ];
      };
      hixFlake = hixProject.flake {};
    in {
      # packages = {
      #   default = hixFlake.packages.default;
      # };
      packages = {
        default = hixFlake.packages."cheeblr-backend:exe:cheeblr-backend" or hixFlake.packages.default;
      } // hixFlake.packages;
      apps = hixFlake.apps;
      checks = hixFlake.checks;
      legacyPackages = pkgs;

      devShell = pkgs.mkShell {
        inherit name;
        inputsFrom = [hixFlake.devShell];
        buildInputs = [
          # PureScript tools
          pkgs.esbuild
          pkgs.nodejs_20
          pkgs.nixpkgs-fmt
          pkgs.purs
          pkgs.purs-tidy
          pkgs.purs-backend-es
          pkgs.purescript-language-server
          pkgs.spago-unstable
          pkgs.zlib
          
          # Shell applications
          # lambda-gen
          spago-watch
          vite
          dev
          pkgs.cabal-install
          code-workspace
        ] ++ (pkgs.lib.optionals (system == "aarch64-darwin")
          (with pkgs.darwin.apple_sdk.frameworks; [
            Cocoa
            CoreServices
          ]));

        shellHook = ''
          echo "Available commands: dev, lambda-gen, spago-watch"
          echo "Using GHC version: $(ghc --version)"
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