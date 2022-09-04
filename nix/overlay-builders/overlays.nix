{pkgs}: let
  inherit (pkgs.lib) composeManyExtensions pipe;
  inherit (pkgs.haskell.lib.compose) dontCheck dontHaddock;

  defaultBuilder = nixPath:
    import ./haskell.nix {
      drvFn = ghcPackages:
        pipe (ghcPackages.callPackage nixPath {}) [
          dontCheck
          dontHaddock
        ];
    };
in {
  default = composeManyExtensions [
    (defaultBuilder ../packages/tasty-json-reporter.nix)
    (defaultBuilder ../packages/tasty-json-markdown.nix)
  ];
}
