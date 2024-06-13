{ config, inputs', pkgs, ... }:

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
        avrdude
        fanbox-dl
        curl
        coreutils-prefixed
        nix-output-monitor
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
        screen
        exiv2
        dua
        doctl
        zstd
        hyperfine
        ffmpeg
        blisp
        picotool
        typst
        typst-lsp
        typst-preview;

      # inherit (inputs'.nixpkgs-f2k.packages) wezterm-git;
      inherit (inputs'.nil.packages) nil;
      inherit (inputs'.nvim.packages) neovim;
    };

    sessionVariables = {
      MODULAR_HOME = "/Users/moni/.local/share/modular";
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
        
        . ${config.age.secrets.tokens.path}
        . /Users/moni/.local/share/miniconda/etc/fish/conf.d/conda.fish
      '';

      shellInit = ''
        fish_add_path /Users/moni/Library/Python/3.11/bin
        fish_add_path -m /Users/moni/.local/share/miniconda/bin
        fish_add_path -amP /Applications/ArmGNUToolchain/13.2.Rel1/arm-none-eabi/bin
        fish_add_path -amP /usr/bin
        fish_add_path -amP /opt/homebrew/bin
        fish_add_path -amP /usr/local/smlnj/bin
        fish_add_path -amP /opt/local/bin
        fish_add_path -amP /opt/homebrew/opt/llvm/bin
        fish_add_path -amP /Users/moni/.local/share/modular/pkg/packages.modular.com_mojo/bin
        fish_add_path -m /run/current-system/sw/bin
        fish_add_path -m /Users/moni/.nix-profile/bin
      '';
    };
  };
}
