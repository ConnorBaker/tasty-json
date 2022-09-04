# Returns an overlay of nixpkgs where each package set for each GHC includes
# the given derivation.
{
  drvFn, # :: GHCPackageSet -> Derivation
}: final: prev: let
  inherit (prev.lib) mapAttrs recursiveUpdateUntil;

  new = {
    haskell.packages =
      mapAttrs
      (
        ghcName: ghcPackages:
          ghcPackages.extend
          (
            _: _: let
              drv = drvFn ghcPackages;
            in {
              "${drv.pname}" = drv;
            }
          )
      )
      prev.haskell.packages;
  };
in
  recursiveUpdateUntil (path: _: _: path == ["haskell" "packages"]) prev new
