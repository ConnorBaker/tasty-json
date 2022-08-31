{
  ghcName,
  composeManyExtensions,
}: let
  tasty-json-reporter = import ./tasty-json-reporter.nix {inherit ghcName;};
  tasty-json-markdown = import ./tasty-json-markdown.nix {inherit ghcName;};
  default = composeManyExtensions [tasty-json-reporter tasty-json-markdown];
in {
  inherit tasty-json-reporter tasty-json-markdown default;
}
