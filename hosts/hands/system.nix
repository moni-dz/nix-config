{
  config,
  lib,
  pkgs,
  ...
}:

{
  age = {
    rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDSEtKx+3rpaqqahgigzvhkaxF7Y7DjOhxg7R3NVjGD9";

    secrets.netrc = {
      rekeyFile = ../../secrets/netrc.age;
      mode = "444";
    };
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      limine = {
        enable = true;
        efiSupport = true;
      };

      efi.canTouchEfiVariables = true;
    };
  };

  environment = {
    enableAllTerminfo = false;
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

  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    hostName = "hands";

    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
  };

  nix.settings.extra-trusted-users = [
    "moni"
    "lyt"
  ];

  nixpkgs.hostPlatform = "x86_64-linux";

  programs.bash.interactiveShellInit = ''
    if [ "$(id -u)" != 0 ] && [ -z "$IN_NUSHELL" ]; then
      export IN_NUSHELL=1
      exec ${lib.getExe pkgs.nushell}
    fi
  '';

  services = {
    openssh.enable = true;
    resolved.enable = true;
    tailscale.enable = true;
  };

  system.stateVersion = "26.05";
  time.timeZone = "Asia/Manila";
}
