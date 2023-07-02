{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.nixosConfigurations;
  configurations = __mapAttrs (_: value: value._nixos) cfg;

  nixosOpts = opts@{ config, name, ... }: {
    options = {
      system = lib.mkOption {
        type = types.enum [ "aarch64-linux" "x86_64-linux" ];
      };

      stateVersion = lib.mkOption {
        type = types.str;
      };

      wsl = lib.mkOption {
        type = types.bool;
        default = false;
      };

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
      };

      _nixos = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
      };
    };

    config._nixos = withSystem config.system (ctx@{ branches, inputs', system, ... }:
      inputs.nixpkgs.lib.nixosSystem {
        modules = config.modules ++ [
          # Shared configuration across all NixOS machines
          ../shared/nixos

          (args@{ lib, pkgs, ... }: {
            inherit (ctx) nixpkgs;

            # Extra arguments passed to the module system
            _module.args = {
              inherit branches inputs' system;
            };

            nix = import ../../nix-settings.nix {
              inherit lib inputs inputs';
              inherit (pkgs) stdenv;
            };

            networking.hostName = name;
            system.stateVersion = config.stateVersion;
          }
          // (args.lib.optionalAttrs config.wsl { wsl.wslConf.network.hostname = name; }))
        ] ++ lib.optional config.wsl inputs.nixos-wsl.nixosModules.wsl;
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
