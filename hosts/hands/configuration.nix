{
  config,
  lib,
  pkgs,
  ...
}:

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

  # Jovian vendors gamescope 3.16.30.1, so drop the nixpkgs patch that is already in it.
  nixpkgs.overlays = [
    (_: prev: {
      jovianPackages = prev.jovianPackages.overrideScope (
        _: super: {
          gamescope = super.gamescope.overrideAttrs (old: {
            patches = builtins.filter (
              p:
              !(builtins.any (s: prev.lib.hasSuffix s (toString p)) [
                # wlroots libinput backport, already in 3.16.30.1
                "c08d99437ec8bb56a703f04ad1ef199502c62d10.diff"
              ])
            ) old.patches;

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
    shell = pkgs.nushell;
    extraGroups = [ "wheel" ];
    packages = [ pkgs.nano ];
  };

  # hjem-rum has no bash module to load per-user session variables, so this is system wide
  environment.variables.EDITOR = "nvim";

  hjem.users.lyt = {
    enable = true;
    imports = [ ../../users/jujutsu.nix ];
    rum.programs.neovim.enable = true;

    # moni's jj config, minus what needs things only that machine has
    rum.programs.jujutsu.settings = {
      fsmonitor.backend = lib.mkForce "none";
      signing.behavior = lib.mkForce "drop";
    };

    rum.programs.nushell = {
      enable = true;
      settings.show_banner = false;

      # nushell doesn't read /etc/set-environment like POSIX shells, so put the NixOS profiles on PATH
      envFile = ''
        $env.path ++= ["/run/wrappers/bin" "/etc/profiles/per-user/lyt/bin" "/run/current-system/sw/bin" "/nix/var/nix/profiles/default/bin"]
      '';
    };
  };
}
