{
  description = "A flake for tasty-json";

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-language-server.url = "github:haskell/haskell-language-server/7760340e999693d07fdbea49c9e20a3dd5458ad3";
    alejandra.url = "github:kamadorueda/alejandra";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    alejandra,
    haskell-language-server,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghcVersion = "924";
      ghcName = "ghc${ghcVersion}";
      hls = haskell-language-server.packages.${system}."haskell-language-server-${ghcVersion}";

      formatter = alejandra.packages.${system}.default;
      overlays = import ./nix/overlay-builders/overlays.nix {
        inherit ghcName;
        inherit (nixpkgs.legacyPackages.${system}.lib) composeManyExtensions;
      };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlays.default];
      };
      haskellPkgs = pkgs.haskell.packages.${ghcName};
      inherit (haskellPkgs) callPackage shellFor tasty-json-reporter tasty-json-markdown;
    in {
      inherit overlays formatter;

      packages = {
        inherit tasty-json-reporter tasty-json-markdown;
        default = tasty-json-reporter;
      };

      devShells.default = shellFor {
        packages = ps: with ps; [tasty-json-reporter tasty-json-markdown];
        buildInputs = [pkgs.cabal-install pkgs.cabal2nix hls];
      };
    });
}
