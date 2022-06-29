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

  home = rec {
    packages = lib.attrValues {
      inherit (pkgs) ripgrep pfetch;
    };

    username = "zero";
    homeDirectory = "/home/${username}";

    /*
      NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.
      Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
    */
    stateVersion = "21.11";
  };

  xdg.configFile."nvim" = {
    recursive = true;
    source = ./config/neovim;
  };
}
