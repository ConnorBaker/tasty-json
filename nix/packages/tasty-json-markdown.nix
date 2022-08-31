{
  mkDerivation,
  base,
  lib,
  tasty-json-reporter,
}:
mkDerivation {
  pname = "tasty-json-markdown";
  version = "0.1.0.0";
  src = ../../tasty-json-markdown;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [base tasty-json-reporter];
  license = "unknown";
  mainProgram = "tasty-json-markdown";
}
