{
  description = "A flake for tasty-json";

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-language-server = {
      url = "github:haskell/haskell-language-server/830596ee212d4f2fbbc81bcf5d08574ae96947d3";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    alejandra = {
      url = "github:kamadorueda/alejandra";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    alejandra,
    haskell-language-server,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      overlays = import ./nix/overlay-builders/overlays.nix {
        pkgs = nixpkgs.legacyPackages.${system};
      };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlays.default];
      };

      inherit (pkgs.haskell.packages.ghc924) shellFor tasty-json-reporter tasty-json-markdown ormolu hlint cabal-install cabal2nix;
      inherit (pkgs.haskell.lib.compose) dontCheck dontHaddock;
      inherit (pkgs.lib) flip pipe;
      hls = haskell-language-server.packages.${system}.haskell-language-server-924;
    in {
      inherit overlays;
      formatter = alejandra.packages.${system}.default;

      apps = {
        update-flake = {
          type = "app";
          program = ./nix/scripts/update-flake.sh;
        };
        update-cabal = {
          type = "app";
          program = ./nix/scripts/update-cabal.sh;
        };
      };

      packages = {
        inherit tasty-json-reporter tasty-json-markdown;
        default = tasty-json-reporter;
      };

      devShells.default = shellFor {
        packages = ps: with ps; [tasty-json-reporter tasty-json-markdown];
        buildInputs = map (flip pipe [dontCheck dontHaddock]) [ormolu hlint cabal-install cabal2nix hls];
      };
    });
}
