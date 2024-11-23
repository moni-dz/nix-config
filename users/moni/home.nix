{
  config,
  inputs',
  pkgs,
  ...
}:

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
        fanbox-dl
        curl
        nix-output-monitor
        parallel
        fd
        ripgrep
        jq
        pfetch
        nixd
        ;

      inherit (inputs'.nil.packages) nil;
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
        fish_add_path -amP /Users/moni/.modular/bin
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
