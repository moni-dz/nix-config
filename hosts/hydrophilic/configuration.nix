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
  imports = [ ./hardware-configuration.nix ];

  boot.loader.limine = {
    enable = true;
    secureBoot.enable = true;
    maxGenerations = 5;

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
    graphics = {
      enable = true;

      extraPackages = __attrValues {
        inherit (pkgs)
          intel-media-driver
          ;
      };
    };

    openrazer = {
      enable = true;
      users = [ "moni" ];
    };

    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.beta;
      open = true;
    };
  };

  programs.firefox = {
    enable = true;

    preferences = {
      "media.ffmpeg.vaapi.enabled" = true;
      "media.hardware-video-decoding.force-enabled" = true;
      "media.rdd-ffmpeg.enabled" = true;
      "media.av1.enabled" = true;
      "gfx.x11-egl.force-enabled" = true;
      "widget.dmabuf.force-enabled" = true;
    };
  };

  environment = {
    sessionVariables.LIBVA_DRIVER_NAME = "nvidia";

    systemPackages = __attrValues {
      inherit (pkgs)
        proton-vpn-cli
        protonvpn-gui
        sbctl
        ;

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
