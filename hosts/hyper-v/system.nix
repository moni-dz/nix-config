{
  config,
  lib,
  pkgs,
  ...
}:

{
  age = {
    rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICCT2klS66tOnx6Lr6ArLvINMrqyNh8Wf8r770/kaCB5";

    secrets = {
      tokens = {
        rekeyFile = ../../secrets/tokens.age;
        owner = "moni";
        mode = "600";
      };

      # `access-tokens = github.com=...`, included from nix.conf
      nix-access-tokens = {
        rekeyFile = ../../secrets/nix-access-tokens.age;
        owner = "moni";
        mode = "600";
      };

      netrc = {
        rekeyFile = ../../secrets/netrc.age;
        mode = "444";
      };
    };
  };

  boot = {
    kexec.enable = false;
    bcache.enable = false;
    initrd.systemd.tpm2.enable = false;

    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 10;
      };

      efi.canTouchEfiVariables = true;
    };
  };

  documentation = {
    nixos.enable = false;
    info.enable = false;
    doc.enable = false;
  };

  environment = {
    defaultPackages = [ ];
    systemPackages = __attrValues { inherit (pkgs) curl gitMinimal; };
    stub-ld.enable = false;
    variables.EDITOR = "nvim";

    etc."determinate/config.json".text = lib.mkForce (
      builtins.toJSON (
        {
          authentication.additionalNetrcSources = [ config.age.secrets.netrc.path ];
        }
        // lib.optionalAttrs (config.determinate.edgeCacheSubstituters != null) {
          inherit (config.determinate) edgeCacheSubstituters;
        }
      )
    );
  };

  fonts.fontconfig.enable = false;
  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    hostName = "hyper-v";
    useDHCP = false;
    useNetworkd = true;
    nftables.enable = true;
  };

  nix = {
    channel.enable = false;

    extraOptions = ''
      !include ${config.age.secrets.nix-access-tokens.path}
    '';

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    settings.extra-trusted-users = [ "moni" ];
  };

  nixpkgs.hostPlatform = "x86_64-linux";
  powerManagement.enable = false;

  programs = {
    nano.enable = false;

    bash.interactiveShellInit = ''
      if [ "$(id -u)" != 0 ] && [ -z "$IN_NUSHELL" ]; then
        export IN_NUSHELL=1
        exec ${lib.getExe pkgs.nushell}
      fi
    '';
  };

  security.wrappers = lib.genAttrs [
    "chsh"
    "mount"
    "newgidmap"
    "newgrp"
    "newuidmap"
    "sg"
    "umount"
  ] (_: lib.mkForce { enable = false; });

  services = {
    openssh.enable = true;
    resolved.enable = true;
    dbus.brokerPackage = pkgs.dbus-broker.override { systemd = config.systemd.package; };
    timesyncd.enable = false;
    nscd.enable = false;
    tailscale.enable = true;
    lvm.enable = false;
    logrotate.enable = false;

    journald.settings.Journal = {
      Audit = false;
      Storage = "volatile";
    };
  };

  system = {
    stateVersion = "26.05";
    nssModules = lib.mkForce [ ];
    tools.nixos-generate-config.enable = false;
  };

  systemd = {
    # systemdMinimal drops the units this host needs, so turn those back on
    package = pkgs.systemdMinimal.override {
      withNetworkd = true;
      withResolved = true;
      withLogind = true;
      withPam = true;
      withVConsole = true;
      withHwdb = true;
      withQrencode = true;
    };

    coredump.enable = false;
    enableEmergencyMode = false;
    oomd.enable = false;

    network = {
      wait-online.enable = false;

      networks."10-lan" = {
        matchConfig.Type = "ether";
        networkConfig.DHCP = "yes";
      };
    };

    generators.systemd-ssh-generator = "/dev/null";
  };

  time.timeZone = "Asia/Manila";

  users.users.moni = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  xdg = {
    autostart.enable = false;
    icons.enable = false;
    menus.enable = false;
    mime.enable = false;
    sounds.enable = false;
  };
}
