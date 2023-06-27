{ config, lib, pkgs, system, inputs, inputs', ... }:

{
  environment = {
    shells = lib.attrValues { inherit (pkgs) fish; };

    systemPackages = lib.attrValues {
      inherit (pkgs) nano git fish home-manager;
      inherit (inputs'.agenix.packages) agenix;
    };
  };

  services = {
    nix-daemon.enable = true;

    skhd = {
      enable = true;

      skhdConfig = ''
        cmd - return : open -na wezterm
      '';
    };
  };

  programs = {
    fish = {
      enable = true;

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
    };
  };

  security.pam.enableSudoTouchIdAuth = true;

  fonts = {
    fontDir.enable = true;

    fonts = lib.attrValues {
      inherit (pkgs) fira-code comic-neue emacs-all-the-icons-fonts;
    };
  };
}
