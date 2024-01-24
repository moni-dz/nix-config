{ config, inputs, inputs', lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  imports = [
    # Append your custom home-manager modules in this list
    # ../../modules/home-manager/wayland/windowManager/river
  ];

  colorscheme = inputs.nix-colors.colorSchemes.material-darker;

  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    font.name = "Sarasa Gothic J";

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "${if config.colorscheme.kind == "light" then "Papirus-Light" else "Papirus-Dark"}";
    };

    theme = {
      name = "phocus";

      package = inputs'.nixpkgs-f2k.packages.phocus-modified.override {
        inherit (config.colorscheme) colors;
        primary = config.colorscheme.colors.base0D;
        secondary = config.colorscheme.colors.base0E;
      };
    };

    gtk2.extraConfig = "gtk-cursor-theme-size=16";
    gtk3.extraConfig."gtk-cursor-theme-size" = 16;
    gtk4.extraConfig."gtk-cursor-theme-size" = 16;
  };

  home = {
    file.".icons/default".source = "${
      if config.colorscheme.kind == "light"
      then "${pkgs.phinger-cursors}/share/icons/phinger-cursors-light"
      else "${pkgs.phinger-cursors}/share/icons/phinger-cursors"
    }";

    packages = __attrValues {
      inherit (pkgs)
        bemenu
        brave
        brightnessctl
        discord-canary
        clang
        celluloid
        ffmpeg
        font-manager
        graphviz
        hydra-check
        hyperfine
        i3a
        imagemagick
        imv
        jq
        lazygit
        libimobiledevice
        libirecovery
        networkmanager_dmenu
        nil
        notify-desktop
        nvd
        wayland-utils
        xdragon
        xdg_utils;

      inherit (pkgs.nodePackages) insect;
      inherit (pkgs.sway-contrib) grimshot;
      inherit (inputs'.agenix.packages) agenix;

      inherit (inputs'.nixpkgs-wayland.packages)
        grim
        slurp
        swaybg
        swayidle
        swaylock
        wf-recorder
        wl-clipboard
        wlogout;

      palette = pkgs.writers.writeDashBin "palette" (import ./scripts/palette.nix);
      volume = pkgs.writers.writeDashBin "volume" (import ./scripts/volume.nix);
    };

    sessionPath = [
      "${config.xdg.configHome}/scripts"
      "${config.home.homeDirectory}/.local/bin"
    ];

    sessionVariables = with config.colorscheme.colors; {
      BEMENU_OPTS = "-H 18 -l 5 --fn 'Iosevka FT QP Light 10.5' --tb '#${base0D}' --tf '#${base02}' --hb '#${base0D}' --hf '#${base02}' --nb '#${base02}' --fb '#${base02}'";
      BROWSER = "brave";
      GOPATH = "${config.home.homeDirectory}/Extras/go";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      RUSTUP_HOME = "${config.home.homeDirectory}/.local/share/rustup";
      XCURSOR_SIZE = "16";
      WLR_NO_HARDWARE_CURSORS = "1";
      NIXOS_OZONE_WL = "1";
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
      MOZ_DISABLE_RDD_SANDBOX = "1";
      MOZ_GLX_TEST_EARLY_WL_ROUNDTRIP = "1";
    };
  };

  programs = {
    alacritty = {
      enable = true;

      settings = import ./config/alacritty.nix {
        inherit (config) colorscheme;
        isWayland = config.wayland.windowManager.sway.enable;
      };
    };

    discocss = {
      enable = false;
      discordAlias = true;
      discordPackage = pkgs.discord-canary;
      css = import ./config/discocss-css.nix { inherit (config) colorscheme; };
    };

    ncspot.enable = true;

    waybar = {
      enable = true;
      package = inputs'.nixpkgs-wayland.packages.waybar;

      settings = [
        {
          layer = "bottom";
          position = "top";
          height = 17;
          modules-left = [ "sway/workspaces" "sway/mode" ];
          modules-right = [ "pulseaudio" "clock" ];
          modules = import ./config/waybar/modules.nix;
        }
      ];

      style = import ./config/waybar/style.nix { inherit (config) colorscheme; };

      systemd = {
        enable = true;
        target = "sway-session.target";
      };
    };

    zathura = {
      enable = true;
      extraConfig = "map <C-i> recolor";
      options = import ./config/zathura.nix { inherit (config) colorscheme; };
    };
  };

  services = {
    dunst = {
      enable = true;

      package = inputs'.nixpkgs-wayland.packages.dunst.overrideAttrs (old: {
        __contentAddressed = true;
      });

      iconTheme = {
        name = "Papirus";
        size = "32x32";
        package = pkgs.papirus-icon-theme;
      };

      settings = import ./config/dunst.nix { inherit (config) colorscheme; };
    };

    swayidle =
      let
        display = status: "swaymsg 'output * power ${status}'";
      in
      {
        enable = true;
        package = inputs'.nixpkgs-wayland.packages.swayidle;

        events = [
          { event = "before-sleep"; command = display "off"; }
          { event = "before-sleep"; command = "swaylock"; }
          { event = "after-resume"; command = display "on"; }
          { event = "lock"; command = display "off"; }
          { event = "unlock"; command = display "on"; }
        ];

        timeouts = [
          { timeout = 300; command = display "off"; resumeCommand = display "on"; }
          { timeout = 310; command = "swaylock"; }
        ];
      };
  };

  wayland.windowManager.sway = {
    enable = true;

    config = with config.colorscheme.colors; rec {
      bars = [ ];
      defaultWorkspace = "workspace number 1";
      modifier = "Mod1";
      terminal = "${config.programs.alacritty.package}/bin/alacritty";
      menu = "bemenu-run ${config.home.sessionVariables.BEMENU_OPTS}";

      colors =
        let
          inactive = {
            background = "#${base02}";
            border = "#${base02}";
            childBorder = "#${base02}";
            indicator = "#${base02}";
            text = "#${base05}";
          };
        in
        {
          inherit (inactive) background;

          focused = {
            background = "#${base0D}";
            border = "#${base0D}";
            childBorder = "#${base0D}";
            indicator = "#${base0D}";
            text = "#${base00}";
          };

          focusedInactive = inactive;
          unfocused = inactive;
          urgent = inactive;
        };

      input = {
        "type:keyboard" = {
          repeat_rate = "40";
          repeat_delay = "350";
        };

        "type:pointer" = {
          accel_profile = "flat";
          pointer_accel = "0.3";
        };

        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
        };
      };

      gaps = {
        outer = -6;
        inner = 19;
        smartGaps = false;
        smartBorders = "no_gaps";
      };

      keybindings =
        let
          grimshot = action: target: "exec grimshot ${action} ${target}";
          volume = action: "exec volume ${action}";
          brightness = action: "exec brightnessctl -q set ${if action == "up" then "10%+" else "10%-"}";
        in
        lib.mkOptionDefault {
          "${modifier}+Return" = "exec ${terminal}";
          "${modifier}+d" = "exec ${menu}";
          "${modifier}+e" = "fullscreen toggle";
          "${modifier}+t" = "floating toggle";
          "${modifier}+q" = "kill";
          "${modifier}+Shift+q" = "exec wlogout -b 2 -c 10 -r 10";
          "${modifier}+Shift+r" = "reload";
          "${modifier}+Print" = grimshot "copy" "area";
          "${modifier}+0" = "workspace number 10";
          "${modifier}+Shift+0" = "move container to workspace number 10";
          "${modifier}+b" = "exec pkill -USR1 waybar";
          "${modifier}+c" = "split h";
          "${modifier}+v" = "split v";
          "${modifier}+p" = "mode passthrough";
          "${modifier}+s" = "exec i3a-swap";
          "Print" = grimshot "copy" "active";
          "Control+Print" = grimshot "copy" "screen";
          "Mod4+Print" = grimshot "save" "screen";
          "XF86AudioRaiseVolume" = volume "up";
          "XF86AudioLowerVolume" = volume "down";
          "XF86AudioMute" = volume "toggle";
          "XF86MonBrightnessUp" = brightness "up";
          "XF86MonBrightnessDown" = brightness "down";
        };

      output."*".bg = "#${base03} solid_color";

      window = {
        border = 4;
        hideEdgeBorders = "smart";
      };
    };

    extraConfig =
      let
        borderImage = ../../assets/border.png;
      in
      ''
        exec_always i3a-master-stack --stack dwm --stack-size 47

        # border_images.unfocused ${borderImage}
        # border_images.focused ${borderImage}
        # border_images.focused_inactive ${borderImage}
        # border_images.urgent ${borderImage}

        mode passthrough bindsym Mod1+p mode default
      '';

    extraSessionCommands = ''
      export XDG_SESSION_DESKTOP=sway
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland-egl
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export MOZ_ENABLE_WAYLAND=1
      export CLUTTER_BACKEND=wayland
      export ECORE_EVAS_ENGINE=wayland-egl
      export ELM_ENGINE=wayland_egl
      export NO_AT_BRIDGE=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';

    wrapperFeatures.gtk = true;
  };

  xdg = {
    configFile = {
      "networkmanager-dmenu/config.ini".source = (pkgs.formats.ini { }).generate "config.ini" {
        dmenu.dmenu_command = "bemenu ${config.home.sessionVariables.BEMENU_OPTS}";
        editor.terminal = "${pkgs.alacritty}/bin/alacritty";
      };

      "swaylock/config".text = ''
        daemonize
        ignore-empty-password
        show-failed-attempts
        color=${config.colorscheme.colors.base00}
        font=Sarasa UI J
      '';

      "wlogout/layout".text = import ./config/wlogout/layout.nix;

      "wlogout/style.css".text = import ./config/wlogout/style.nix {
        inherit (config) colorscheme;
        iconsDirectory =
          let inherit (inputs'.nixpkgs-wayland.packages) wlogout;
          in "${wlogout}/share/wlogout/icons";
      };
    };

    userDirs = {
      enable = true;
      documents = "${config.home.homeDirectory}/Extras/Documents";
      music = "${config.home.homeDirectory}/Media/Music";
      pictures = "${config.home.homeDirectory}/Media/Pictures";
      videos = "${config.home.homeDirectory}/Media/Videos";
    };
  };
}
