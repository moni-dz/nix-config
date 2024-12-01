{
  description = "Stub flake for dev partition";
  nixConfig.commit-lockfile-summary = "flake: bump dev inputs";
  outputs = _: { };
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";
}
