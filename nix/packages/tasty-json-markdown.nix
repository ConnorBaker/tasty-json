{
  mkDerivation,
  aeson,
  base,
  bytestring,
  lib,
  tasty,
  tasty-json-reporter,
  unordered-containers,
}:
mkDerivation {
  pname = "tasty-json-markdown";
  version = "0.1.0.0";
  src = ../../tasty-json-markdown;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    tasty
    tasty-json-reporter
    unordered-containers
  ];
  license = "unknown";
  mainProgram = "tasty-json-markdown";
}
