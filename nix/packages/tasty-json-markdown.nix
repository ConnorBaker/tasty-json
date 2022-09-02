{
  mkDerivation,
  aeson,
  base,
  bytestring,
  lib,
  tasty-json-reporter,
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
    tasty-json-reporter
  ];
  license = "unknown";
  mainProgram = "tasty-json-markdown";
}
