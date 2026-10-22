{
  config,
  lib,
  inputs,
  withSystem,
  ...
}:

let
  inherit lib;
  inherit (lib) types;

  nixosOpts =
    { config, name, ... }:
    {
      options = {
        system = lib.mkOption {
          type = types.enum [
            "aarch64-linux"
            "x86_64-linux"
          ];
          description = "System architecture for the configuration.";
        };

        stateVersion = lib.mkOption {
          type = types.str;
          description = "NixOS state version, changing this value DOES NOT update your system.";
        };

        determinate = lib.mkEnableOption "Determinate Nix";

        modules = lib.mkOption {
          type = types.listOf types.unspecified;
          description = "List of NixOS modules to include in the configuration.";
        };

        _nixos = lib.mkOption {
          type = types.unspecified;
          readOnly = true;
          description = "Composed NixOS configuration.";
        };
      };

      config._nixos = withSystem config.system (
        ctx:
        inputs.nixpkgs.lib.nixosSystem {
          modules =
            config.modules
            ++ lib.optionals config.determinate [
              inputs.determinate.nixosModules.default
            ]
            ++ [
              (
                { pkgs, ... }:
                {
                  inherit (ctx) nix nixpkgs;
                  _module.args = ctx.extraModuleArgs;
                  networking.hostName = name;

                  system = {
                    inherit (config) stateVersion;
                  };

                  hjem = {
                    clobberByDefault = true;
                    specialArgs = ctx.extraModuleArgs;
                    extraModules = [
                      inputs.hjem-rum.hjemModules.default
                      ../hjem
                    ];
                  };

                  environment = {
                    enableAllTerminfo = false;
                    systemPackages = ctx.basePackagesFor pkgs;
                  };
                }
              )
            ];
        }
      );
    };
in
{
  options.parts.nixosConfigurations = lib.mkOption {
    type = types.attrsOf (types.submodule nixosOpts);
  };

  config.flake.nixosConfigurations = __mapAttrs (
    _: value: value._nixos
  ) config.parts.nixosConfigurations;
}
