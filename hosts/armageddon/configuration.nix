{ config, lib, pkgs, system, inputs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = lib.attrValues {
    inherit (pkgs) discord git vim;
    inherit (inputs.agenix.packages.${system}) agenix;
  };

  services = {
    nix-daemon.enable = true;

    skhd = {
      enable = true;

      skhdConfig = ''
        cmd - down : yabai -m window --focus south
        cmd - up : yabai -m window --focus north
        cmd - left : yabai -m window --focus west
        cmd - right : yabai -m window --focus east
        shift + cmd - r : yabai -m space --rotate 270
        shift + cmd - y : yabai -m space --mirror y-axis
        shift + cmd - x : yabai -m space --mirror x-axis
        shift + cmd - t : yabai -m window --toggle float --grid 4:4:1:1:2:2
        cmd - e : yabai -m window --toggle zoom-fullscreen
        shift + cmd - e : yabai -m space --balance
        shift + cmd - left : yabai -m window --space prev;
        shift + cmd - right : yabai -m window --space next;
        shift + cmd - 1 : yabai -m window --space 1;
        shift + cmd - 2 : yabai -m window --space 2;
        shift + cmd - 3 : yabai -m window --space 3;
        shift + cmd - 4 : yabai -m window --space 4;
        shift + cmd - 5 : yabai -m window --space 5;
        shift + cmd - 6 : yabai -m window --space 6;
        shift + cmd - 7 : yabai -m window --space 7;
        shift + cmd - 8 : yabai -m window --space 8;
        shift + cmd - 9 : yabai -m window --space 9;
        shift + cmd - 0 : yabai -m window --space 0;
        cmd - return : open -na wezterm
      '';
    };

    yabai = {
      enable = true;
      enableScriptingAddition = true;

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
        window_opacity = "off";
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
    zsh = {
      enable = true;

      interactiveShellInit = ''
        . ${config.age.secrets.github-token.path}
      '';
    };
  };

  security.pam.enableSudoTouchIdAuth = true;

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      upgrade = true;
    };

    casks = [
      "appcleaner"
      "raycast"
    ];
  };

  fonts = {
    fontDir.enable = true;
    fonts = lib.attrValues { inherit (pkgs) iosevka-ft; };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
