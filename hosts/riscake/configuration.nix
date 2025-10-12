{
  config,
  lib,
  pkgs,
  inputs',
  ...
}:

{
  system.primaryUser = "moni";

  environment = {
    shells = __attrValues { inherit (pkgs) fish nushell; };

    systemPackages = __attrValues {
      inherit (inputs'.agenix.packages) agenix;
      inherit (pkgs) nushell;
    };
  };

  services = {
    skhd = {
      enable = false;

      skhdConfig =
        let
          ws = n: "cmd - ${n} : yabai -m space --focus ${n}";
          move = n: "shift + cmd - ${n} : yabai -m window --space ${n}; yabai -m space --focus ${n}";
        in
        ''
          cmd - return : ^open -na ghostty
        ''
        + lib.optionalString config.services.yabai.enable ''
          cmd - e : yabai -m window --toggle zoom-fullscreen
          shift + cmd - e : yabai -m window --toggle float --grid 4:4:1:1:2:2
        ''
        + lib.optionalString config.services.yabai.enable (
          lib.concatLines (map move (map toString (lib.range 1 6)))
        )
        + lib.optionalString config.services.yabai.enable (
          lib.concatLines (map ws (map toString (lib.range 1 6)))
        );
    };

    yabai = {
      enable = false;
      enableScriptingAddition = false;

      config =
        let
          gap = 8;
        in
        {
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
        yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
        yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'
        yabai -m rule --add app='System Settings' manage=off
        yabai -m rule --add app='Finder' manage=off
      '';
    };
  };

  programs.fish.enable = true;

  security.pam.services.sudo_local = {
    reattach = true;
    touchIdAuth = true;
    watchIdAuth = true;
  };

  fonts.packages = __attrValues {
    inherit (pkgs) comic-neue;
  };
}
