{ config, inputs, lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://rycee.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://rycee.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages =
      let
        stdenv = pkgs.stdenvAdapters.withCFlags [ "-O3" "-pipe" "-mcpu=apple-m1" ] pkgs.llvmPackages_latest.stdenv;
      in
      lib.attrValues {
        inherit (pkgs)
          asitop
          curlFull
          coreutils-prefixed
          fd
          gnugrep
          shellcheck
          ripgrep
          jq
          pfetch
          pandoc
          helix
          sqlite
          typst
          typst-lsp
          typst-fmt
          libheif
          avrdude
          screen
          colima
          xdelta
          docker
          rizin
          protobuf
          exiv2;

        inherit (inputs.nixpkgs-f2k.packages.${system}) wezterm-git;

        ffmpeg = pkgs.ffmpeg_6-headless.override {
          inherit stdenv;
        };

        #emacs-plus = inputs.nixpkgs-f2k.packages.${system}.emacs-plus-git.override {
        #  inherit stdenv;
        #};
      };

    sessionVariables.EDITOR = "hx";
  };

  programs = {
    fish = {
      enable = true;
      shellAbbrs = import ../shared/config/sh-aliases.nix;

      shellInit = ''
        fish_add_path /Users/moni/.cargo/bin
        fish_add_path /Users/moni/Library/Python/3.11/bin
        fish_add_path -m /opt/homebrew/bin
        fish_add_path -m /run/current-system/sw/bin
        fish_add_path -m /Users/moni/.nix-profile/bin
      '';

      interactiveShellInit = ''
        set -U fish_greeting
      '';
    };

    zoxide = {
      enable = true;
      enableFishIntegration = config.programs.fish.enable;
    };
  };
}
