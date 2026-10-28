{ inputs, ... }:

{
  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem =
    { inputs', ... }:
    let
      pkgs = inputs'.nixpkgs.legacyPackages;
      inherit (inputs.nixpkgs.lib) fileset;

      fennelFiles = fileset.difference (fileset.fileFilter (
        file: file.hasExt "fnl" || file.hasExt "fnlm"
      ) ../.) ../modules/hjem/rex/types/rex-docset.fnl;
    in
    {
      checks.fennel-lambda =
        pkgs.runCommand "fennel-lambda"
          {
            nativeBuildInputs = [ pkgs.luaPackages.fennel ];

            src = fileset.toSource {
              root = ../.;
              fileset = fennelFiles;
            };
          }
          ''
            cd $src
            fennel ${./check-lambda.fnl} $(find . -name '*.fnl' -o -name '*.fnlm')
            touch $out
          '';

      treefmt = {
        projectRootFile = "flake.nix";
        programs.nixfmt.enable = true;

        settings.global.excludes = [
          "LICENSE"
          ".github/CODEOWNERS"
          "*.age"
          "*.jpg"
          "*.png"
          "*.toml"
          "*.yml"
          ".jj/*"
        ];
      };
    };
}
