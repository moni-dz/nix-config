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

  boot = {
    kernelModules = [ "amd-pstate" ];

    kernelParams = [
      "amd_pstate=passive"
      "amd_pstate.shared_mem=1"
      "initcall_blacklist=acpi_cpufreq_init"
    ];
  };

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
      open = true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
    };

    opengl = {
      enable = true;
      driSupport = true;
    };
  };

  # Font packages should go in fonts.fonts in ../shared/configuration.nix.
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      WLR_NO_HARDWARE_CURSORS = "1";
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
      MOZ_DISABLE_RDD_SANDBOX = "1";
      MOZ_GLX_TEST_EARLY_WL_ROUNDTRIP = "1";
    };

    systemPackages = lib.attrValues {
      inherit (pkgs)
        file
        ntfs3g
        nvtop-nvidia
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

  fonts.enableDefaultFonts = false;

  # https://github.com/nix-community/home-manager/issues/1288#issuecomment-636352427
  programs.sway.enable = true;
  programs.zsh.enable = true;

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
