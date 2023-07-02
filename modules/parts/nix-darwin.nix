{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.darwinConfigurations;
  configurations = __mapAttrs (_: value: value._darwin) cfg;

  darwinOpts = { config, name, ... }: {
    options = {
      system = lib.mkOption {
        type = types.enum [ "aarch64-darwin" "x86_64-darwin" ];
      };

      stateVersion = lib.mkOption {
        type = types.int;
      };

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
      };

      _darwin = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
      };
    };

    config._darwin = withSystem config.system ({ branches, inputs', system, ... }@args:
      inputs.darwin.lib.darwinSystem {
        inherit inputs system;

        modules = config.modules ++ [
          ({ lib, pkgs, ... }: {
            nixpkgs = builtins.removeAttrs args.nixpkgs [ "hostPlatform" ];

            # Extra arguments passed to the module system
            _module.args = {
              inherit branches inputs' system;
              inputs = lib.mkForce inputs;
            };

            nix = import ../../nix-settings.nix {
              inherit lib inputs inputs';
              inherit (pkgs) stdenv;
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
  options.parts.darwinConfigurations = lib.mkOption {
    type = types.attrsOf (types.submodule darwinOpts);
  };

  config.flake.darwinConfigurations = configurations;
}
