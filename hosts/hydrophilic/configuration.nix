# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.limine = {
    enable = true;
    secureBoot.enable = true;

    extraEntries = ''
      /Windows
        protocol: efi
        path: uuid(61b09bee-2f93-4700-ba06-19f2f1a03c5b):/EFI/Microsoft/Boot/bootmgfw.efi
    '';
  };

  services = {
    desktopManager.plasma6.enable = true;

    displayManager.sddm = {
      enable = true;

      wayland = {
        enable = true;
        compositor = "kwin";
      };
    };

    xserver = {
      enable = false;
      videoDrivers = [ "nvidia" ];
    };
  };

  hardware = {
    graphics.enable = true;

    openrazer = {
      enable = true;
      users = [ "moni" ];
    };

    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.beta;
      open = true;
    };
  };

  programs.firefox.enable = true;

  environment.systemPackages = with pkgs; [
    ghostty
    sbctl
  ];
}
