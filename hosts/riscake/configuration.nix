{
  config,
  lib,
  pkgs,
  system,
  self,
  inputs,
  inputs',
  ...
}:

{
  environment = {
    shells = __attrValues { inherit (pkgs) fish; };

    systemPackages = __attrValues {
      inherit (inputs'.agenix.packages) agenix;
    };
  };

  services.nix-daemon.enable = true;
  programs.fish.enable = true;
  security.pam.enableSudoTouchIdAuth = true;

  fonts.packages = __attrValues {
    inherit (pkgs) comic-neue iosevka-ft;
  };
}
