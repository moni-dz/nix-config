{ config, lib, pkgs, system, inputs, ... }:

{
  environment = {
    shells = lib.attrValues { inherit (pkgs) fish; };

    systemPackages = lib.attrValues {
      inherit (pkgs) nano git fish home-manager;
      inherit (inputs.agenix.packages.${system}) agenix;
    };
  };

  services = {
    mysql = {
      enable = true;
      package = pkgs.mariadb_1011;
      dataDir = "/Users/moni/.dbdata/mariadb";
    };

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
      inherit (pkgs) fira-code iosevka-ft emacs-all-the-icons-fonts;
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
