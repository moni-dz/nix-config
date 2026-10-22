{
  self,
  inputs,
  withSystem,
  ...
}:

/*
  Shared profiles go after the host's own modules: list options (e.g. `extra-trusted-users`,
  `environment.systemPackages`) merge in module order, and this keeps the order the systems had
  when the builder appended them.
*/
{
  # Steam Deck (Jovian-NixOS)
  flake.nixosConfigurations.hands = withSystem "x86_64-linux" (
    { inputs', self', ... }:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit
          inputs
          self
          inputs'
          self'
          ;
      };
      modules = [
        inputs.agenix.nixosModules.default
        inputs.jovian.nixosModules.default
        inputs.rex.nixosModules.default
        ../../modules/common/identity.nix
        ../../modules/nixos/hjem.nix
        (
          {
            config,
            lib,
            pkgs,
            ...
          }:

          {
            networking.hostName = "hands";
            nixpkgs.hostPlatform = "x86_64-linux";
            system.stateVersion = "26.05";

            age.secrets.netrc = {
              file = ../../secrets/netrc.age;
              mode = "444";
            };

            nix.settings = {
              netrc-file = config.age.secrets.netrc.path;
              extra-trusted-users = [
                "moni"
                "lyt"
              ];
            };

            boot = {
              kernelPackages = pkgs.linuxPackages_latest;

              loader = {
                systemd-boot.enable = true;
                efi.canTouchEfiVariables = true;
              };
            };

            networking.networkmanager = {
              enable = true;
              dns = "systemd-resolved";
            };

            time.timeZone = "Asia/Manila";
            i18n.defaultLocale = "en_US.UTF-8";

            services = {
              openssh.enable = true;
              resolved.enable = true;
              tailscale.enable = true;
            };

            programs.bash.interactiveShellInit = ''
              if [ "$(id -u)" != 0 ] && [ -z "$IN_NUSHELL" ]; then
                export IN_NUSHELL=1
                exec ${lib.getExe pkgs.nushell}
              fi
            '';

            environment = {
              enableAllTerminfo = false;
              variables.EDITOR = "nvim";
            };
          }
        )
        ./hardware.nix
        ./desktop.nix
        ./rex.nix
        ./lyt.nix
        ../../profiles/identity.nix
        ../../profiles/nixpkgs.nix
        ../../profiles/nix.nix
        ../../profiles/hjem-system.nix
        ../../profiles/base-packages.nix
      ];
    }
  );
}
