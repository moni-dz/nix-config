{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # world readable like the old /etc/nix/netrc, since flake inputs are fetched by the client (lyt)
  age.secrets.netrc = {
    file = ../../secrets/netrc.age;
    mode = "444";
  };

  nix.settings = {
    netrc-file = config.age.secrets.netrc.path;
    extra-trusted-users = [ "lyt" ];
  };

  # Jovian vendors gamescope 3.16.30.1, but nixpkgs' patches target an older release.
  # Drop the ones that no longer apply; GetUsrDir moved to src/Utils/DirHelpers.cpp.
  nixpkgs.overlays = [
    (_: prev: {
      jovianPackages = prev.jovianPackages.overrideScope (
        _: super: {
          gamescope = super.gamescope.overrideAttrs (old: {
            patches = builtins.filter (
              p:
              !(builtins.any (s: prev.lib.hasSuffix s (toString p)) [
                "shaders-path.patch"
                # wlroots libinput backport, already in 3.16.30.1
                "c08d99437ec8bb56a703f04ad1ef199502c62d10.diff"
              ])
            ) old.patches;

            postPatch =
              builtins.replaceStrings
                [ ''substituteInPlace src/reshade_effect_manager.cpp --replace-fail "@out@" "$out"'' ]
                [ ''substituteInPlace src/Utils/DirHelpers.cpp --replace-fail '"/usr"' "\"$out\""'' ]
                old.postPatch;

            # unit tests need catch2
            mesonFlags = old.mesonFlags ++ [ (prev.lib.mesonBool "enable_tests" false) ];
          });
        }
      );
    })
  ];

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
    desktopManager.plasma6.enable = true;
    openssh.enable = true;
    resolved.enable = true;
    tailscale.enable = true;

    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
    };

    rex.loginServer = {
      enable = true;
      accounts.lyt.principals = [ "tailscale:moni-dz@github" ];
      tailscaleServe.enable = true;
    };
  };

  jovian.steam = {
    enable = true;
    autoStart = true;
    user = "lyt";
    desktopSession = "plasma";
  };

  users.users.lyt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    packages = [ pkgs.nano ];
  };
}
