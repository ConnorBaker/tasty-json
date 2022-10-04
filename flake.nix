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
    
    hix.url = "github:tek/hix";
    hix.inputs.nixpkgs.follows = "nixpkgs";
    
    haskell-language-server.url = "github:haskell/haskell-language-server/5d35a740b2fd70b225aaa94541ff22c3ed3e060a";
    haskell-language-server.inputs.nixpkgs.follows = "nixpkgs";
    
    alejandra.url = "github:kamadorueda/alejandra";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    hix,
    haskell-language-server,
    alejandra,
  }:
    hix.lib.auto ({lib, ...}: {
      # output.systems = [ "aarch64-darwin" ];
      # compat.enable = true;
      # shell.hls.package = haskell-language-server.packages.aarch64-darwin.haskell-language-server-924;
      packages = {
        tasty-json-reporter = {
          src = ./tasty-json-reporter;
          dependencies = [
            "aeson"
            "base"
            "bytestring"
            "containers"
            "stm"
            "tagged"
            "tasty"
            "text"
            "unordered-containers"
          ];
        };
      };
    });
    
    # flake-utils.lib.eachDefaultSystem (system: let
    #   overlays = import ./nix/overlays.nix;

    #   pkgs = import nixpkgs {
    #     inherit system;
    #     overlays = [overlays.default];
    #   };

    #   inherit (pkgs.haskell.packages.ghc924) shellFor tasty-json-reporter tasty-json-markdown ormolu hlint cabal-install cabal2nix;
    #   inherit (pkgs.haskell.lib.compose) dontCheck dontHaddock;
    #   inherit (pkgs.lib) flip pipe;
    #   hls = haskell-language-server.packages.${system}.haskell-language-server-924;
    # in {
    #   inherit overlays;
    #   formatter = alejandra.packages.${system}.default;

    #   apps = {
    #     update-flake = {
    #       type = "app";
    #       program = ./nix/scripts/update-flake.sh;
    #     };
    #     update-cabal = {
    #       type = "app";
    #       program = ./nix/scripts/update-cabal.sh;
    #     };
    #   };

    #   packages = {
    #     inherit tasty-json-reporter tasty-json-markdown;
    #     default = tasty-json-reporter;
    #   };

    #   devShells.default = shellFor {
    #     packages = ps: with ps; [tasty-json-reporter tasty-json-markdown];
    #     buildInputs = map (flip pipe [dontCheck dontHaddock]) [ormolu hlint cabal-install cabal2nix hls];
    #   };
    # });
}
