{ config, inputs', lib, pkgs, system, ... }:

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
        stdenv = pkgs.stdenvAdapters.withCFlags [ "-Ofast" "-pipe" "-mcpu=apple-m1" ] pkgs.llvmPackages_latest.stdenv;
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
          exiv2
          ffmpeg_6;

        sdrpp = pkgs.sdrpp.override {
          inherit stdenv;

          fftwFloat = pkgs.fftwFloat.overrideAttrs (_: {
            inherit stdenv;
            postPatch = null;
          });
        };

        inherit (inputs'.nixpkgs-f2k.packages) wezterm-git;
        inherit (inputs'.nixd.packages) nixd;
      };

    sessionVariables.EDITOR = "hx";
  };

  programs = {
    fish.shellInit = ''
      fish_add_path /Users/moni/.cargo/bin
      fish_add_path /Users/moni/Library/Python/3.11/bin
      fish_add_path /usr/local/bin
      fish_add_path -m /opt/homebrew/bin
      fish_add_path /opt/local/bin
      fish_add_path -m /run/current-system/sw/bin
      fish_add_path -m /Users/moni/.nix-profile/bin
    '';
  };
}
