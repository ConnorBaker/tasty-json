{ pkgs }:
let
  inherit (pkgs.lib) attrNames composeManyExtensions;
  ghcNames = attrNames pkgs.haskell.packages;
  mkOverlays = ghcName:
    let
      tasty-json-reporter = import ./haskell.nix {
        inherit ghcName;
        cabal2nixifiedDrvPath = ../packages/tasty-json-reporter.nix;
      };
      tasty-json-markdown = import ./haskell.nix {
        inherit ghcName;
        cabal2nixifiedDrvPath = ../packages/tasty-json-markdown.nix;
      };
      default = composeManyExtensions [ tasty-json-reporter tasty-json-markdown ];
    in
    {
      inherit tasty-json-reporter tasty-json-markdown default;
    };

  overlays = builtins.listToAttrs (map (ghcName: {name = ghcName; value = mkOverlays ghcName;}) ghcNames);
  default = composeManyExtensions (map (ghcName: (mkOverlays ghcName).default) ghcNames);
in
overlays // {inherit default;}