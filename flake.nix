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
      main = "tasty-json-reporter";
      packages.tasty-json-reporter = ./tasty-json-reporter;
      packages.tasty-json-reporter-test = ./tasty-json-reporter-test;
    });
}
