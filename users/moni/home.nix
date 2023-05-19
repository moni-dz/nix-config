{ config, inputs, lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://rycee.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://rycee.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = lib.attrValues {
      inherit (pkgs)
        asitop
        coreutils-prefixed
        fd
        ffmpeg
        gnugrep
        shellcheck
        ripgrep
        jq
        #discord
        pfetch
        pandoc
        vscode
        helix
        sqlite
        typst
        typst-lsp
        typst-fmt
        libheif
        exiv2;

      inherit (inputs.nixpkgs-f2k.packages.${system}) emacs-plus-git wezterm-git;
    };

    sessionVariables.EDITOR = "hx";
  };

  programs = {
    discocss = {
      enable = false;
      discordAlias = false;
      css = builtins.readFile ./config/rose-pine.theme.css;
    };

    fish = {
      enable = true;
      shellAbbrs = import ../shared/config/sh-aliases.nix;

      shellInit = ''
        fish_add_path /Users/moni/Library/Python/3.11/bin
        fish_add_path /opt/local/bin
        fish_add_path /run/current-system/sw/bin
      '';

      interactiveShellInit = ''
        set -U fish_greeting
      '';
    };
  };
}
