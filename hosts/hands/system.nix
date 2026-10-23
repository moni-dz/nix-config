{
  config,
  lib,
  pkgs,
  ...
}:

{
  networking.hostName = "hands";
  nixpkgs.hostPlatform = "x86_64-linux";
  system.stateVersion = "26.05";

  age.secrets.netrc = {
    file = ../../secrets/netrc.age;
    mode = "444";
  };

  nix.settings.extra-trusted-users = [
    "moni"
    "lyt"
  ];

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

  networking.networkmanager = {
    enable = true;
    dns = "systemd-resolved";
  };

  time.timeZone = "Asia/Manila";
  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    openssh.enable = true;
    resolved.enable = true;
    tailscale.enable = true;
  };

  programs.bash.interactiveShellInit = ''
    if [ "$(id -u)" != 0 ] && [ -z "$IN_NUSHELL" ]; then
      export IN_NUSHELL=1
      exec ${lib.getExe pkgs.nushell}
    fi
  '';

  environment = {
    enableAllTerminfo = false;
    variables.EDITOR = "nvim";
  };
}
