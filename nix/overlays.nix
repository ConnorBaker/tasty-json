{
  default = final: prev: let
    inherit (prev.lib) pipe recursiveUpdateUntil;
    inherit (prev.haskell.lib.compose) dontCheck dontHaddock;
    ghc902 = prev.haskell.packages.ghc902.extend (hself: hsuper: {
      tasty-json-reporter = pipe (hsuper.callPackage ./packages/tasty-json-reporter.nix {}) [
        dontCheck
        dontHaddock
      ];
      tasty-json-markdown = pipe (hsuper.callPackage ./packages/tasty-json-markdown.nix {}) [
        dontCheck
        dontHaddock
      ];
    });
    ghc924 = prev.haskell.packages.ghc924.extend (hself: hsuper: {
      tasty-json-reporter = pipe (hsuper.callPackage ./packages/tasty-json-reporter.nix {}) [
        dontCheck
        dontHaddock
      ];
      tasty-json-markdown = pipe (hsuper.callPackage ./packages/tasty-json-markdown.nix {}) [
        dontCheck
        dontHaddock
      ];
    });
    new.haskell.packages = {
      inherit ghc902 ghc924;
    };
  in
    recursiveUpdateUntil (path: _: _: path == ["haskell" "packages" "ghc924"]) prev new;
}
