{ config, lib, pkgs, system, self, inputs, inputs', ... }:

{
  environment = {
    shells = lib.attrValues { inherit (pkgs) fish; };

    systemPackages = lib.attrValues {
      inherit (pkgs) nano git fish home-manager;
      inherit (inputs'.agenix.packages) agenix;

      activate-builder =
        let
          darwin-builder = inputs.nixpkgs.lib.nixosSystem {
            system = __replaceStrings [ "darwin" ] [ "linux" ] system;

            modules = [
              (nixos@{ modulesPath, lib, ... }: {
                imports = [ "${nixos.modulesPath}/profiles/macos-builder.nix" ];
                system.nixos.revision = nixos.lib.mkForce null;

                virtualisation = {
                  host.pkgs = pkgs;
                  darwin-builder.memorySize = 1024 * 8;
                };
              })
            ];
          };
        in
        darwin-builder.config.system.build.macos-builder-installer;
    };
  };

  services = {
    nix-daemon.enable = true;

    skhd = {
      enable = true;

      skhdConfig = ''
        cmd - return : open -na wezterm
      '';
    };
  };

  programs.fish.enable = true;
  security.pam.enableSudoTouchIdAuth = true;

  fonts = {
    fontDir.enable = true;

    fonts = lib.attrValues {
      inherit (pkgs) fira-code comic-neue emacs-all-the-icons-fonts;
    };
  };
}
