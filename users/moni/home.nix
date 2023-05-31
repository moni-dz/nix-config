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
          docker
          kitty
          exiv2;

        inherit (inputs.nixpkgs-f2k.packages.${system}) wezterm-git;

        ffmpeg = pkgs.ffmpeg_6-headless.override {
          inherit stdenv;
        };

        emacs-plus = inputs.nixpkgs-f2k.packages.${system}.emacs-plus-git.override {
          inherit stdenv;
        };
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
        fish_add_path /opt/local/bin
        fish_add_path /run/current-system/sw/bin
        fish_add_path /Users/moni/.nix-profile/bin
      '';

      interactiveShellInit = ''
        set -U fish_greeting
        set ZJ_SESSIONS (zellij list-sessions)
        set NO_SESSIONS (echo "$ZJ_SESSIONS" | wc -l)

        if not set -q ZELLIJ
          if [ "$NO_SESSIONS" -ge 2 ]
            zellij -l compact a "$(echo "$ZJ_SESSIONS" | ${lib.getExe pkgs.skim})"
          else
            zellij -l compact a -c main
          end
        end
      '';
    };

    zellij = {
      enable = true;

      settings = {
        simplified_ui = true;
        pane_frames = false;
        auto_layout = false;
        theme = "rose-pine-dawn";

        themes.rose-pine-dawn = {
          fg = "#faf4ed";
          bg = "#575279";

          red = "#eb6f92";
          green = "#31748f";
          blue = "#9ccfd8";
          yellow = "#f6c177";
          magenta = "#c4a7e7";
          orange = "#fe640b";
          cyan = "#ebbcba";
          black = "#222137";
          white = "#e0def4";
        };
      };
    };
  };
}
