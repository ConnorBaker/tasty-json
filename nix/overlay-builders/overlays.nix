{
  ghcName,
  composeManyExtensions,
}: let
  tasty-json-reporter = import ./haskell.nix {
    inherit ghcName;
    cabal2nixifiedDrvPath = ../packages/tasty-json-reporter.nix;
  };
  tasty-json-markdown = import ./haskell.nix {
    inherit ghcName;
    cabal2nixifiedDrvPath = ../packages/tasty-json-markdown.nix;
  };
  default = composeManyExtensions [tasty-json-reporter tasty-json-markdown];
in {
  inherit tasty-json-reporter tasty-json-markdown default;
}
