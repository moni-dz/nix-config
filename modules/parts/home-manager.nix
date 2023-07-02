{ config, lib, inputs, withSystem, ... }:

let
  inherit lib;
  inherit (lib) types;

  cfg = config.parts.homeConfigurations;
  configurations = __mapAttrs (_: value: value._home) cfg;

  homeOpts = opts@{ config, lib, name, ... }: {
    options = {
      system = lib.mkOption {
        type = types.enum [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];
      };

      agenix = lib.mkEnableOption "agenix";

      stateVersion = lib.mkOption {
        type = types.str;
      };

      modules = lib.mkOption {
        type = types.listOf types.unspecified;
      };

      _home = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
      };
    };

    config._home = withSystem config.system (ctx@{ branches, inputs', system, ... }:
      inputs.home.lib.homeManagerConfiguration {
        # Default nixpkgs for home.nix
        pkgs = inputs.nixpkgs.legacyPackages.${system};

        modules = config.modules ++ [
          # Shared configuration across all users
          ../shared/home-manager

          (args@{ config, lib, ... }: {
            nixpkgs = builtins.removeAttrs ctx.nixpkgs [ "hostPlatform" ];

            # Extra arguments passed to the module system
            _module.args = {
              inherit branches inputs inputs' system;
            };

            home = {
              inherit (opts.config) stateVersion;
              username = name;

              homeDirectory =
                if args.lib.hasSuffix "darwin" system
                then "/Users/${name}"
                else "/home/${name}";
            };
          } // (args.lib.optionalAttrs opts.config.agenix {
            age.identityPaths = [
              "${config.home.homeDirectory}/.ssh/id_ed25519"
              "/etc/ssh/ssh_host_ed25519_key"
            ];
          }))
        ] ++ lib.optional opts.config.agenix inputs.agenix.homeManagerModules.age;
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
