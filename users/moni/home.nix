{ config, inputs', lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages =
      let
        stdenv = pkgs.stdenvAdapters.withCFlags [ "-Ofast" "-pipe" "-mcpu=apple-m1" ] pkgs.llvmPackages_latest.stdenv;
      in
      lib.attrValues {
        inherit (pkgs)
          asitop
          curlFull
          coreutils-prefixed
          parallel
          fd
          gnugrep
          shellcheck
          ripgrep
          jq
          pfetch
          pandoc
          helix
          sqlite
          libheif
          avrdude
          screen
          exiv2
          dua
          pigz
          hyperfine
          ffmpeg_6;

        sdrpp = pkgs.sdrpp.override {
          inherit stdenv;

          fftwFloat = pkgs.fftwFloat.overrideAttrs (_: {
            inherit stdenv;
            postPatch = null;
          });
        };

        inherit (inputs'.nixpkgs-f2k.packages) wezterm-git;
      };

    sessionVariables.EDITOR = "hx";
  };

  programs = {
    fish = {
      interactiveShellInit = ''
        function export
          if [ $argv ] 
            set var (echo $argv | cut -f1 -d=)
            set val (echo $argv | cut -f2 -d=)
            set -g -x $var $val
          else
            echo 'export var=value'
          end
        end
        
        . ${config.age.secrets.github-token.path}
      '';

      shellInit = ''
        fish_add_path /Users/moni/Library/Python/3.11/bin
        fish_add_path /usr/local/bin
        fish_add_path -m /opt/homebrew/bin
        fish_add_path /opt/local/bin
        fish_add_path -m /run/current-system/sw/bin
        fish_add_path -m /Users/moni/.nix-profile/bin
      '';
    };
  };
}
