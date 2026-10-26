{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  nix-tools = pkgs.callPackage "${inputs.darwin}/pkgs/nix-tools" {
    git = pkgs.gitMinimal;
    inherit (config.system) profile;
    inherit (config.environment) systemPath;
    nixPath = lib.optionalString config.nix.enable (lib.concatStringsSep ":" config.nix.nixPath);
    nixPackage = if config.nix.enable then config.nix.package else null;
  };
in
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
    shells = __attrValues { inherit (pkgs) nushell; };

    systemPackages = __attrValues {
      inherit (pkgs) nushell;
      inherit (nix-tools) darwin-option darwin-rebuild darwin-version;
    };
  };

  documentation.info.enable = false;

  system.tools = {
    darwin-option.enable = false;
    darwin-rebuild.enable = false;
    darwin-version.enable = false;
  };

  security.pam.services.sudo_local = {
    reattach = true;
    touchIdAuth = true;
    watchIdAuth = true;
  };

  fonts.packages = __attrValues {
    inherit (pkgs) comic-neue;
  };
}
