{
  ghcName,
  drv,
}: final: prev: let
  haskellPkgs = prev.haskell.packages.${ghcName};
  inherit (prev.lib) recursiveUpdateUntil;

  new = {
    haskell.packages.${ghcName} = haskellPkgs.extend (_: _: {
      "${drv.pname}" = drv;
    });
  };
in
  recursiveUpdateUntil (path: l: r: path == ["haskell" "packages" ghcName]) prev new
