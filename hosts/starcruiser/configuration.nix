{ config, lib, pkgs, options, inputs, system, ... }:

/*
  NixOS configuration

  Useful links:
  - Package Search: https://search.nixos.org/packages?channel=unstable
  - Options Search: https://search.nixos.org/options?channel=unstable
*/
{
  imports = [
    ./hardware-configuration.nix

    # Shared configuration across all machines
    ../shared/configuration.nix

    # Append your custom NixOS modules in this list
    ../../modules/nixos/programs/river.nix
  ];

  /*
    NOTE: amd_pstate has a performance hit on Zen 2 and 3 processors
    see: https://github.com/torvalds/linux/blob/23c296f/drivers/cpufreq/amd-pstate.c#L52-L60

    This machine has a Ryzen 5 3600, and enabling amd_pstate caps my performance to 50%.
  */
  boot.kernelParams = [ "amd_pstate.shared_mem=1" ];

  hardware = {
    cpu.amd.updateMicrocode = true;

    /*
      hardware-configuration.nix enables this by default because of this line:

      >  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

      See https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/installer/scan/not-detected.nix
    */
    enableRedistributableFirmware = true;

    nvidia = {
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.latest;
    };

    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [ nvidia-vaapi-driver ];
      extraPackages32 = with pkgs; [ nvidia-vaapi-driver ];
    };
  };

  # Font packages should go in fonts.fonts in ../shared/configuration.nix.
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      WLR_NO_HARDWARE_CURSORS = "1";
    };

    systemPackages = lib.attrValues {
      inherit (pkgs)
        file
        ntfs3g
        pavucontrol
        pulseaudio
        ripgrep
        util-linux
        unrar
        unzip
        xarchiver
        zip;

      inherit (pkgs.qt5) qtwayland;
      inherit (pkgs.gnome3) nautilus;
    };
  };

  services = {
    greetd = {
      enable = true;

      settings = {
        default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd 'sway --unsupported-gpu'";

        initial_session = {
          command = "sway --unsupported-gpu";
          user = "moni";
        };
      };
    };

    xserver.videoDrivers = [ "nvidia" ];
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
