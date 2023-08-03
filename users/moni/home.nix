{ config, inputs', lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = __attrValues {
      inherit (pkgs)
        asitop
        curl
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
        zstd
        hyperfine
        ffmpeg_6
        blisp;

      sdrpp = pkgs.sdrpp.override {
        stdenv = pkgs.appleM2Stdenv;

        fftwFloat = pkgs.fftwFloat.overrideAttrs {
          stdenv = pkgs.appleM2Stdenv;
          postPatch = null;
        };
      };

      inherit (inputs'.nixpkgs-f2k.packages) wezterm-git;
    };
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
        fish_add_path -m /usr/bin
        fish_add_path -m /run/current-system/sw/bin
        fish_add_path -m /Users/moni/.nix-profile/bin
      '';
    };
  };
}
