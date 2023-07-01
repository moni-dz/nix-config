{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.homeConfigurations;
  configurations = __mapAttrs (_: value: value._home) cfg;

  homeOpts = { config, lib, name, ... }: {
    options = {
      system = lib.mkOption {
        type = types.enum [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];
      };

      stateVersion = lib.mkOption {
        type = types.str;
      };

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
      };

      _home = lib.mkOption {
        type = lib.types.unspecified;
        readOnly = true;
      };
    };

    config._home = withSystem config.system ({ inputs', system, nixpkgs-config, overlays, ... }@args:
      inputs.home.lib.homeManagerConfiguration {
        # Default nixpkgs for home.nix
        pkgs = inputs.nixpkgs.legacyPackages.${system};

        modules = config.modules ++ [
          # Shared configuration across all users
          ../../users/shared

          ({ lib, ... }: {
            # Extra arguments passed to the module system
            _module.args = {
              inherit inputs inputs' system;
              inherit (args) master unstable stable;
            };

            nixpkgs = {
              inherit overlays;
              config = nixpkgs-config;
            };

            home = {
              inherit (config) stateVersion;
              username = name;

              homeDirectory =
                if lib.hasSuffix "darwin" system
                then "/Users/${name}"
                else "/home/${name}";
            };

          })
        ];
      }
    );
  };
in
{
  options.parts.homeConfigurations = lib.mkOption {
    type = types.attrsOf (types.submodule homeOpts);
  };

  config.flake.homeConfigurations = configurations;
}
