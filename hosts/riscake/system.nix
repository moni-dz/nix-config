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
  determinateNix = {
    # Determinate Nix reads its settings from here, not `nix.settings`
    customSettings.extra-trusted-users = [ "moni" ];
  };

  documentation.info.enable = false;

  environment = {
    shells = __attrValues { inherit (pkgs) nushell; };

    systemPackages = __attrValues {
      inherit (pkgs) nushell;
      inherit (nix-tools) darwin-option darwin-rebuild darwin-version;
    };

    # `customSettings` can only render `key = value`, so append the include ourselves
    etc."nix/nix.custom.conf".text = lib.mkAfter ''
      !include ${config.age.secrets.nix-access-tokens.path}
    '';
  };

  fonts.packages = __attrValues {
    inherit (pkgs) comic-neue;
  };

  networking.hostName = "riscake";
  nixpkgs.hostPlatform = "aarch64-darwin";

  security.pam.services.sudo_local = {
    reattach = true;
    touchIdAuth = true;
    watchIdAuth = true;
  };

  system = {
    stateVersion = 5; # only change this if you know what you are doing.
    primaryUser = "moni";

    tools = {
      darwin-option.enable = false;
      darwin-rebuild.enable = false;
      darwin-version.enable = false;
    };
  };
}
