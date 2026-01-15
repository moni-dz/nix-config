{
  infuse,
  inputs,
  inputs',
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    ./kernel.nix
  ];

  boot.loader = {
    timeout = 30;
    limine = {
      enable = true;
      secureBoot.enable = true;
      maxGenerations = 5;

      extraEntries = ''
        /Windows
          protocol: efi
          path: uuid(61b09bee-2f93-4700-ba06-19f2f1a03c5b):/EFI/Microsoft/Boot/bootmgfw.efi
          comment: Windows Boot Manager
      '';

      style = {
        wallpapers = [ ];

        graphicalTerminal = {
          palette = "3b4252;df8c8f;a9a874;c1ad89;7ca8dd;a9a0bf;6fb1ba;e5e9f0";
          brightPalette = "4c566a;df8c8c;a9a874;c1ad89;7ca8dd;a9a0bf;6fb1ba;eceff4";
          background = "1a1a1a";
          foreground = "d8dee9";
          brightBackground = "4c566a";
          brightForeground = "eceff4";
        };
      };
    };
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

    xserver.videoDrivers = lib.mkForce [ "nvidia" ];
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
      modesetting.enable = true;

      prime = {
        sync.enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
    };
  };

  programs = {
    firefox = {
      enable = true;
      package = pkgs.firefox-devedition;

      preferences =
        let
          v = config.programs.firefox.package.version;
        in
        {
          "media.ffmpeg.vaapi.enabled" = lib.versionOlder v "137.0.0";
          "media.hardware-video-decoding.force-enabled" = lib.versionAtLeast v "137.0.0";
          "media.rdd-ffmpeg.enabled" = lib.versionOlder v "97.0.0";
          "media.av1.enabled" = true;
          "gfx.x11-egl.force-enabled" = true;
          "widget.dmabuf.force-enabled" = true;
          "gfx.webrender.all" = true;
        };
    };

    partition-manager.enable = true;
  };

  environment = {
    variables = {
      MOZ_DISABLE_RDD_SANDBOX = "1";
      MOZ_DRM_DEVICE = "/dev/dri/card0";
    };

    sessionVariables = {
      LIBVA_DRIVER_NAME = "nvidia";
      NVD_BACKEND = "direct";
    };

    systemPackages = __attrValues {
      inherit (pkgs)
        proton-vpn-cli
        protonvpn-gui
        sbctl
        libva-utils
        zed-editor
        ;

      nvtop = pkgs.nvtopPackages.full;

      nixpkgs-review = infuse pkgs.nixpkgs-review {
        __input.nix.__assign = config.nix.package;
      };

      ghostty =
        (inputs'.ghostty.packages.ghostty.override { enableX11 = false; }).overrideAttrs
          (prevAttrs: {
            zigBuildFlags = [
              "--system"
              "${prevAttrs.deps}"
              "-Dversion-string=${prevAttrs.version}-${inputs.ghostty.rev}-nix"
              "-Dgtk-x11=false"
              "-Dgtk-wayland=true"
              "-Dcpu=raptorlake"
              "-Doptimize=ReleaseFast"
              "-Dstrip=true"
              "--color off"
            ];
          });
    };
  };
}
