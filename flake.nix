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
      output.systems = ["aarch64-darwin"]; 
      devGhc.compiler = "ghc902";
      main = "tasty-json-reporter";
      packages.tasty-json-reporter = ./tasty-json-reporter;
      packages.tasty-json-reporter-test = ./tasty-json-reporter-test;
    });
}
