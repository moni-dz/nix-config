{
  config,
  lib,
  pkgs,
  ...
}:

{
  networking.hostName = "hyper-v";
  nixpkgs.hostPlatform = "x86_64-linux";
  system.stateVersion = "26.05";

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

  environment.etc."determinate/config.json".text = lib.mkForce (
    builtins.toJSON (
      {
        authentication.additionalNetrcSources = [ config.age.secrets.netrc.path ];
      }
      // lib.optionalAttrs (config.determinate.edgeCacheSubstituters != null) {
        inherit (config.determinate) edgeCacheSubstituters;
      }
    )
  );

  nix.extraOptions = ''
    !include ${config.age.secrets.nix-access-tokens.path}
  '';

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    useDHCP = false;
    useNetworkd = true;
  };

  systemd.network.networks."10-lan" = {
    matchConfig.Type = "ether";
    networkConfig.DHCP = "yes";
  };

  time.timeZone = "Asia/Manila";
  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    openssh.enable = true;
    resolved.enable = true;
    nscd.enable = false;
    tailscale.enable = true;
  };

  users.users.moni = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  system.nssModules = lib.mkForce [ ];
  systemd.oomd.enable = false;

  nix.settings.extra-trusted-users = [ "moni" ];
  environment.variables.EDITOR = "nvim";

  programs.bash.interactiveShellInit = ''
    if [ "$(id -u)" != 0 ] && [ -z "$IN_NUSHELL" ]; then
      export IN_NUSHELL=1
      exec ${lib.getExe pkgs.nushell}
    fi
  '';
}
