{
  description = "A flake for tasty-json";

  inputs = {
    hix.url = "github:tek/hix";
    nixpkgs.follows = "hix/nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    hix,
  }:
    hix.lib.flake ({ 
      base = ./.;
      output.systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"]; 
      devGhc.compiler = "ghc902";
      packages.tasty-json-reporter = ./.;
      overrides.all = {notest, ...}: {
        tasty-json-reporter = notest;
      };
    });
}
