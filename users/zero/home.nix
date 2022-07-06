{ config, inputs, lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://rycee.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://rycee.gitlab.io/home-manager/options.html
*/
{
  imports = [
    # Shared configuration across all users
    ../shared/home.nix
  ];

  home.packages = lib.attrValues {
    inherit (pkgs) ripgrep pfetch;
  };

  xdg.configFile."nvim" = {
    recursive = true;
    source = ./config/neovim;
  };
}
