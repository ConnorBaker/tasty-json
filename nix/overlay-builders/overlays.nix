{pkgs}: let
  inherit (pkgs.lib) composeManyExtensions pipe;
  inherit (pkgs.haskell.lib.compose) dontCheck dontHaddock;

  defaultBuilder = nixPath: ghcPackages:
    pipe (ghcPackages.callPackage nixPath {}) [
      dontCheck
      dontHaddock
    ];

  tasty-json-reporter = import ./haskell.nix {
    drvFn = defaultBuilder ../packages/tasty-json-reporter.nix;
  };
  tasty-json-markdown = import ./haskell.nix {
    drvFn = defaultBuilder ../packages/tasty-json-markdown.nix;
  };
  default = composeManyExtensions [tasty-json-reporter tasty-json-markdown];
in {
  inherit tasty-json-reporter tasty-json-markdown default;
}
