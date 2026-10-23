{ inputs, ... }:

{
  imports = [ inputs.agenix-rekey.flakeModule ];

  perSystem =
    { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        packages = [ config.agenix-rekey.package ];
      };
    };
}
