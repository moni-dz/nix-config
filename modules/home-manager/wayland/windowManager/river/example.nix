{ config, inputs, lib, pkgs, system, ... }:

{
  wayland.windowManager.river = {
    enable = false;

    extraSessionVariables = {
      XDG_SESSION_DESKTOP = "sway";
      SDL_VIDEODRIVER = "wayland";
      QT_QPA_PLATFORM = "wayland-egl";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      MOZ_ENABLE_WAYLAND = "1";
      CLUTTER_BACKEND = "wayland";
      ECORE_EVAS_ENGINE = "wayland-egl";
      ELM_ENGINE = "wayland_egl";
      NO_AT_BRIDGE = "1";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };

    config = with config.colorscheme.colors; {
      backgroundColor = base03;

      border = {
        color = {
          focused = base0D;
          unfocused = base02;
          urgent = base08;
        };

        width = 4;
      };

      focusFollowsCursor = true;

      layoutGenerator.arguments = "-view-padding 8 -outer-padding 8";

      keybindings =
        let
          layoutCmd = cmd: "send-layout-cmd ${config.wayland.windowManager.river.config.layoutGenerator.name} '${cmd}'";
        in
        lib.mkOptionDefault {
          normal = {
            "Alt Return" = "spawn alacritty";
            "Alt D" = ''spawn "bemenu-run ${config.home.sessionVariables.BEMENU_OPTS}"'';
            "Alt Q" = "close";
            "Alt+Shift Q" = "exit";
            "Alt S" = "swap next";
            "Alt E" = "toggle-fullscreen";
            "Alt T" = "toggle-float";
            "Alt P" = "enter-mode passthrough";
            "Alt Minus" = layoutCmd "main-ratio -0.05";
            "Alt Equal" = layoutCmd "main-ratio +0.05";
            "Alt Period" = layoutCmd "main-count +1";
            "Alt Comma" = layoutCmd "main-count -1";
          };

          passthrough."Alt P" = "enter-mode normal";

          pointer = {
            "Alt BTN_LEFT" = "move-view";
            "Alt BTN_RIGHT" = "resize-view";
          };
        };
    };

    extraConfig = ''
      for i in $(seq 1 9)
      do
        tags=$((1 << ($i - 1)))

        # Super+[1-9] to focus tag [0-8]
        riverctl map normal Alt $i set-focused-tags $tags

        # Super+Shift+[1-9] to tag focused view with tag [0-8]
        riverctl map normal Alt+Shift $i set-view-tags $tags

        # Super+Ctrl+[1-9] to toggle focus of tag [0-8]
        riverctl map normal Alt+Control $i toggle-focused-tags $tags

        # Super+Shift+Ctrl+[1-9] to toggle tag [0-8] of focused view
        riverctl map normal Alt+Shift+Control $i toggle-view-tags $tags
      done

      all_tags=$(((1 << 32) - 1))
      riverctl map normal Alt 0 set-focused-tags $all_tags
      riverctl map normal Alt+Shift 0 set-view-tags $all_tags
    '';
  };
}
