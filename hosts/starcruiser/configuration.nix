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
    ../shared

    # Append your custom NixOS modules in this list
    ../../modules/nixos/programs/river.nix
  ];


  hardware = {
    cpu.amd.updateMicrocode = true;

    /*
      hardware-configuration.nix enables this by default because of this line:

      >  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

      See https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/installer/scan/not-detected.nix
    */
    enableRedistributableFirmware = true;
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;

    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [ vaapiVdpau libvdpau-va-gl ];
    };
  };

  # Font packages should go in fonts.fonts a few lines below this.
  environment.systemPackages = lib.attrValues {
    inherit (pkgs)
      brightnessctl
      coreutils
      curl
      dash
      fd
      file
      home-manager
      man-pages
      man-pages-posix
      ntfs3g
      pavucontrol
      pulseaudio
      ripgrep
      util-linux
      unrar
      unzip
      wget
      xarchiver
      zip;

    inherit (pkgs.qt5) qtwayland;
    inherit (pkgs.gnome3) nautilus;

    git = pkgs.git.overrideAttrs (_: { /* __contentAddressed = true; */ });
    subversion = pkgs.subversion.overrideAttrs (_: { /* __contentAddressed = true; */ });
  };

  fonts = {
    fonts = lib.attrValues {
      inherit (pkgs)
        emacs-all-the-icons-fonts
        fantasque-sans-mono
        # NOTE: use only when current is outdated
        # iosevka-ft
        # iosevka-ft-qp
        sarasa-gothic
        symbola
        terminus_font
        twemoji-color-font;

      inherit (inputs.nixpkgs-f2k.packages.${system})
        iosevka-ft-bin
        iosevka-ft-qp-bin;

      nerdfonts = pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" "Iosevka" ]; };
    };

    fontconfig = {
      enable = true;

      defaultFonts = {
        serif = [
          "Sarasa Gothic C"
          "Sarasa Gothic J"
          "Sarasa Gothic K"
        ];

        sansSerif = [
          "Sarasa Gothic C"
          "Sarasa Gothic J"
          "Sarasa Gothic K"
        ];

        monospace = [
          "Iosevka FT"
          "Iosevka Nerd Font"
          "Sarasa Mono C"
          "Sarasa Mono J"
          "Sarasa Mono K"
        ];

        emoji = [ "Twitter Color Emoji" ];
      };
    };
  };

  networking = {
    hostName = "starcruiser";
    nameservers = [ "1.1.1.1" "1.0.0.1" "8.8.8.8" "8.8.4.4" ];
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

  /*
    NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

    Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
  */
  system.stateVersion = "22.05";

  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
    wlr.enable = true;
  };
}
