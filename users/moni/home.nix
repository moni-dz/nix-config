{ config, inputs, lib, pkgs, system, age, ... }:

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

    inputs.doom.hmModule
  ];

  home = {
    packages = lib.attrValues {
      inherit (pkgs)
        coreutils-prefixed
        fd
        ffmpeg
        gnugrep
        shellcheck
        ripgrep
        discord
        pfetch
        pandoc
        vscode
        helix
        typst
        typst-lsp
        typst-fmt
        libheif
        exiv2;

      inherit (inputs.nixpkgs-f2k.packages.${system}) emacs-plus-git wezterm-git;
    };

    sessionVariables.EDITOR = "hx";
    stateVersion = "22.11";
  };

  programs = {
    discocss = {
      enable = true;
      discordAlias = false;
      css = builtins.readFile ./config/rose-pine.theme.css;
    };

    fish = {
      enable = true;
      shellAbbrs = import ../shared/config/sh-aliases.nix;

      shellInit = ''
        fish_add_path /Users/moni/Library/Python/3.11/bin
        fish_add_path /opt/local/bin
      '';

      interactiveShellInit = ''
        set -U fish_greeting
      '';
    };
  };
}
