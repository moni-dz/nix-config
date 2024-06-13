{ config, lib, pkgs, system, self, inputs, inputs', ... }:

{
  environment = {
    shells = __attrValues { inherit (pkgs) fish; };

    systemPackages = __attrValues {
      inherit (pkgs) gawk;
      inherit (inputs'.agenix.packages) agenix;
    };
  };

  services = {
    nix-daemon.enable = true;

    skhd = {
      enable = true;

      skhdConfig =
        let
          spaces = map toString (lib.range 1 5);
          ws = n: "cmd - ${n} : yabai -m space --focus ${n}";
          move = n: "shift + cmd - ${n} : yabai -m window --space ${n}; yabai -m space --focus ${n}";
        in
        ''
          cmd - return : open -na iterm
        ''
        + lib.optionalString config.services.yabai.enable ''
          cmd - e : yabai -m window --toggle zoom-fullscreen
          shift + cmd - e : yabai -m window --toggle float --grid 4:4:1:1:2:2
        ''
        + lib.optionalString config.services.yabai.enable (lib.concatLines (map move spaces));
    };

    yabai = {
      enable = true;
      enableScriptingAddition = true;
      package = pkgs.yabai;

      config = let gap = 8; in {
        layout = "bsp";
        mouse_follows_focus = "off";
        top_padding = gap;
        bottom_padding = gap;
        left_padding = gap;
        right_padding = gap;
        window_gap = gap;
        window_placement = "second_child";
        window_shadow = "float";
      };

      extraConfig = ''
        yabai -m rule --add app='System Settings' manage=off
        yabai -m rule --add app='Finder' manage=off
      '';
    };
  };

  programs.fish.enable = true;
  security.pam.enableSudoTouchIdAuth = true;

  fonts = {
    fontDir.enable = true;

    fonts = __attrValues {
      inherit (pkgs)
        fira-code
        comic-neue
        fantasque-sans-mono
        maple-mono-otf
        emacs-all-the-icons-fonts
        terminus_font_ttf
        unifont
        unifont_upper;
    };
  };
}
