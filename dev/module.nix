{ inputs, ... }:

{
  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem = _: {
    treefmt = {
      projectRootFile = "flake.nix";
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
        ".jj/*"
        "nvim/*"
      ];
    };
  };
}
