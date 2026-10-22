{
  config,
  pkgs,
  inputs',
  ...
}:

{
  system.primaryUser = "moni";

  # merged into Determinate's netrc, see https://docs.determinate.systems/determinate-nix/determinate-nixd/
  determinateNix.determinateNixd.authentication.additionalNetrcSources = [
    config.age.secrets.netrc.path
  ];

  environment = {
    shells = __attrValues { inherit (pkgs) fish nushell; };

    systemPackages = __attrValues {
      inherit (inputs'.agenix.packages) agenix;
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
