{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.nixosConfigurations;
  configurations = __mapAttrs (_: value: value._nixos) cfg;

  nixosOpts = { config, name, ... }: {
    options = {
      system = lib.mkOption {
        type = types.enum [ "aarch64-linux" "x86_64-linux" ];
      };

      stateVersion = lib.mkOption {
        type = types.str;
      };

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
      };

      _nixos = lib.mkOption {
        type = lib.types.unspecified;
        readOnly = true;
      };
    };

    config._nixos = withSystem config.system ({ inputs', system, nixpkgs-config, overlays, ... }@args:
      inputs.nixpkgs.lib.nixosSystem {
        modules = config.modules ++ [
          ({ lib, pkgs, ... }: {
            # Extra arguments passed to the module system
            _module.args = {
              inherit inputs' system;
              inherit (args) master unstable stable;
            };

            nix = import ../../nix-settings.nix {
              inherit lib inputs inputs';
              inherit (pkgs) stdenv;
            };

            nixpkgs = {
              inherit overlays;
              config = nixpkgs-config;
              hostPlatform = system;
            };

            networking.hostName = name;
            system.stateVersion = config.stateVersion;
          })
        ];
      }
    );
  };
in
{
  options.parts.nixosConfigurations = lib.mkOption {
    type = types.attrsOf (types.submodule nixosOpts);
  };

  config.flake.nixosConfigurations = configurations;
}
