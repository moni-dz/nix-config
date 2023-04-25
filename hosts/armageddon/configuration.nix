{ config, lib, pkgs, system, inputs, ... }:

{
  environment = {
    shells = lib.attrValues { inherit (pkgs) fish; };
  
    systemPackages = lib.attrValues {
      inherit (pkgs) git fish;
      inherit (inputs.agenix.packages.${system}) agenix;
    };
  };

  services = {
    mysql = {
      enable = true;
      package = pkgs.mariadb_1011;
      dataDir = "/Users/moni/.mariadb";
    };
    
    nix-daemon.enable = true;

    skhd = {
      enable = true;

      skhdConfig = ''
        cmd - return : open -na wezterm
      '' + lib.optionalString config.services.yabai.enable ''
        cmd - down : yabai -m window --focus south
        cmd - up : yabai -m window --focus north
        cmd - left : yabai -m window --focus west
        cmd - right : yabai -m window --focus east
        alt - r : yabai -m space --rotate 270
        shift + cmd - y : yabai -m space --mirror y-axis
        shift + cmd - x : yabai -m space --mirror x-axis
        cmd - t : yabai -m window --toggle float --grid 4:4:1:1:2:2
        cmd - e : yabai -m window --toggle zoom-fullscreen
        shift + cmd - e : yabai -m space --balance
        shift + cmd - left : yabai -m window --space prev
        shift + cmd - right : yabai -m window --space next
        shift + cmd - 1 : yabai -m window --space 1
        shift + cmd - 2 : yabai -m window --space 2
        shift + cmd - 3 : yabai -m window --space 3
        shift + cmd - 4 : yabai -m window --space 4
        shift + cmd - 5 : yabai -m window --space 5
        shift + cmd - 6 : yabai -m window --space 6
        shift + cmd - 7 : yabai -m window --space 7
        shift + cmd - 8 : yabai -m window --space 8
        shift + cmd - 9 : yabai -m window --space 9
        shift + cmd - 0 : yabai -m window --space 10
        cmd - return : open -na wezterm
        shift + cmd - w : yabai -m window --swap recent
        :: resize @
        resize < shift + cmd - r; default
        default < shift + cmd - r; resize
        resize < left : yabai -m window --resize left:-50:0; yabai -m window --resize right:-50:0
        resize < down : yabai -m window --resize bottom:0:50; yabai -m window --resize top:0:50
        resize < up : yabai -m window --resize top:0:-50; yabai -m window --resize bottom:0:-50
        resize < right : yabai -m window --resize right:50:0; yabai -m window --resize left:50:0
      '' + lib.optionalString (config.services.yabai.enable && config.services.yabai.enableScriptingAddition) ''
        cmd - 1 : yabai -m space --focus 1
        cmd - 2 : yabai -m space --focus 2
        cmd - 3 : yabai -m space --focus 3
        cmd - 4 : yabai -m space --focus 4
        cmd - 5 : yabai -m space --focus 5
        cmd - 6 : yabai -m space --focus 6
        cmd - 7 : yabai -m space --focus 7
        cmd - 8 : yabai -m space --focus 8
        cmd - 9 : yabai -m space --focus 9
        cmd - 0 : yabai -m space --focus 10
      '';
    };

    yabai = {
      enable = false;
      enableScriptingAddition = false;

      package = pkgs.yabai.overrideAttrs (_: {
        src = pkgs.fetchzip {
          url = "https://github.com/koekeishiya/yabai/releases/download/v5.0.3/yabai-v5.0.3.tar.gz";
          sha256 = "sha256-dnUrdCbEN/M4RAr/GH3x10bfr2TUjuomxIUStFK7X9M=";
        };
      });

      config = {
        layout = "bsp";
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "off";
        window_placement = "second_child";
        top_padding = 10;
        bottom_padding = 10;
        left_padding = 10;
        right_padding = 10;
        window_gap = 10;
      };

      extraConfig = ''
        yabai -m rule --add app="^System Settings$" manage=off
        yabai -m rule --add app="^Calculator$" manage=off
        yabai -m rule --add app="^Karabiner-Elements$" manage=off
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
      inherit (pkgs) iosevka-ft emacs-all-the-icons-fonts;
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
