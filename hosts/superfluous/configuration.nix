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
    blacklistedKernelModules = [ "amdgpu" ];

    kernelParams = [
      "mitigations=off"
      "acpi_backlight=vendor"
      "nmi_watchdog=0"
      "systemd.watchdog-device=/dev/watchdog"
    ];
  };

  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;

      settings = {
        General.FastConnectable = true;

        Policy = {
          ReconnectAttempts = 7;
          ReconnectUUIDs = "00001124-0000-1000-8000-00805f9b34fb";
        };
      };
    };

    cpu.amd.updateMicrocode = true;

    /*
      hardware-configuration.nix enables this by default because of this line:

      >  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

      See https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/installer/scan/not-detected.nix
    */
    enableRedistributableFirmware = true;

    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [ vaapiVdpau libvdpau-va-gl ];
    };
  };

  # Font packages should go in `fonts.fonts` in ../shared/configuration.nix.
  environment.systemPackages = lib.attrValues {
    inherit (pkgs)
      brightnessctl
      curl
      file
      ntfs3g
      pavucontrol
      pulseaudio
      util-linux
      unrar
      unzip
      xarchiver
      zip;

    inherit (pkgs.qt5) qtwayland;
    inherit (pkgs.gnome3) nautilus;
  };

  networking = {
    dhcpcd.enable = false;
    hostName = "superfluous";

    # Replace with your interface names.
    interfaces = {
      eno1.useDHCP = true;
      wlan0.useDHCP = true;
    };

    networkmanager = {
      enable = true;
      dns = "none";
      wifi.backend = "iwd";
    };

    useDHCP = false;
    wireless.iwd.settings.Settings.AutoConnect = true;
  };

  services.greetd = {
    enable = true;

    settings = {
      default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd 'sway'";

      initial_session = {
        command = "sway";
        user = "fortuneteller2k";
      };
    };
  };

  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
    wlr.enable = true;
  };
}
