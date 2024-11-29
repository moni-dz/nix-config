{ inputs, ... }:

{
  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem = _: {
    treefmt = {
      projectRootFile = ".git/config";
      programs.nixfmt.enable = true;

      settings.global.excludes = [
        "LICENSE"
        ".github/CODEOWNERS"
        "*.age"
        "*.org"
        "*.jpg"
        "*.png"
        "*.toml"
        "*.yml"
      ];
    };
  };
}
