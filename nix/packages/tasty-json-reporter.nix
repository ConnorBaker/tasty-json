{
  mkDerivation,
  aeson,
  base,
  bytestring,
  containers,
  lib,
  stm,
  tagged,
  tasty,
  text,
  unordered-containers,
}:
mkDerivation {
  pname = "tasty-json-reporter";
  version = "0.1.0.0";
  src = ../../tasty-json-reporter;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    containers
    stm
    tagged
    tasty
    text
    unordered-containers
  ];
  license = "unknown";
}
