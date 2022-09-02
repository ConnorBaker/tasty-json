{
  ghcName,
  cabal2nixifiedDrvPath,
}: final: prev: let
  haskellPkgs = prev.haskell.packages.${ghcName};
  inherit (prev.lib) recursiveUpdateUntil;
  inherit (prev.haskell.lib.compose) dontCheck dontHaddock failOnAllWarnings;
  inherit (prev.lib) pipe;

  drv = pipe (haskellPkgs.callPackage cabal2nixifiedDrvPath {}) [
    dontHaddock
    dontCheck
  ];

  new = {
    haskell.packages.${ghcName} = haskellPkgs.extend (_: _: {
      "${drv.pname}" = drv;
    });
  };
in
  recursiveUpdateUntil (path: l: r: path == ["haskell" "packages" ghcName]) prev new
