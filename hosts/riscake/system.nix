{
  config,
  lib,
  pkgs,
  ...
}:

{
  networking.hostName = "riscake";
  nixpkgs.hostPlatform = "aarch64-darwin";

  system = {
    stateVersion = 5; # only change this if you know what you are doing.
    primaryUser = "moni";
  };

  determinateNix = {
    # merged into Determinate's netrc, see https://docs.determinate.systems/determinate-nix/determinate-nixd/
    determinateNixd.authentication.additionalNetrcSources = [ config.age.secrets.netrc.path ];
    # Determinate Nix reads its settings from here, not `nix.settings`
    customSettings.extra-trusted-users = [ "moni" ];
  };

  # `customSettings` can only render `key = value`, so append the include ourselves
  environment.etc."nix/nix.custom.conf".text = lib.mkAfter ''
    !include ${config.age.secrets.nix-access-tokens.path}
  '';

  environment = {
    shells = __attrValues { inherit (pkgs) fish nushell; };

    systemPackages = __attrValues {
      inherit (pkgs) nushell;
    };
  };

  programs.fish.enable = true;

  security.pam.services.sudo_local = {
    reattach = true;
    touchIdAuth = true;
    watchIdAuth = true;
  };

  fonts.packages = __attrValues {
    inherit (pkgs) comic-neue;
  };
}
