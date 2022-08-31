{ghcName}: final: prev: let
  inherit (prev.haskell.packages.${ghcName}) callPackage;
  inherit (prev.haskell.lib.compose) dontCheck dontHaddock failOnAllWarnings;
  inherit (prev.lib) pipe;
in
  import ./haskell.nix {
    inherit ghcName;
    drv = pipe (callPackage ../packages/tasty-json-reporter.nix {}) [
      dontHaddock
      dontCheck
      failOnAllWarnings
    ];
  }
  final
  prev
