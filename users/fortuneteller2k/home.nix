{ config, inputs, lib, pkgs, system, ... }:

/*
  home-manager configuration

  Useful links:
  - Home Manager Manual: https://rycee.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://rycee.gitlab.io/home-manager/options.html
*/
{
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

      package = inputs.nixpkgs-f2k.packages.${system}.phocus.override {
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

    packages = lib.attrValues {
      inherit (pkgs)
        bemenu
        brave-nightly
        brightnessctl
        clang
        celluloid
        dragon-drop
        ffmpeg
        font-manager
        graphviz
        hydra-check
        hyperfine
        # i3a
        imagemagick
        imv
        jq
        lazygit
        libimobiledevice
        libirecovery
        networkmanager_dmenu
        nixpkgs-fmt
        nixpkgs-review
        notify-desktop
        nvd
        rnix-lsp
        wayland-utils
        xdg_utils;

      inherit (pkgs.nodePackages) insect;
      inherit (pkgs.gitAndTools) gh;
      inherit (pkgs.sway-contrib) grimshot;
      inherit (inputs.agenix.packages.${system}) agenix;

      inherit (inputs.nixpkgs-wayland.packages.${system})
        grim
        slurp
        swaybg
        swayidle
        swaylock
        wf-recorder
        wl-clipboard
        wlogout;

      inherit (inputs.i3a.legacyPackages.${system}) i3a;
      inherit (config.programs.neovim) package;

      palette = pkgs.writers.writeDashBin "palette" (import ./scripts/palette.nix);

      pls =
        let
          writePython310Bin = name: pkgs.writers.makePythonWriter pkgs.python310 pkgs.python310Packages "/bin/${name}";
        in
        writePython310Bin "pls" { flakeIgnore = [ "E501" ]; } (import ./scripts/pls.nix);

      python3 =
        let
          extraPythonPackages = pypkgs: with pypkgs; [ i3ipc ];
        in
        pkgs.python310.withPackages extraPythonPackages;

      volume = pkgs.writers.writeDashBin "volume" (import ./scripts/volume.nix);
    };

    sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
      "${config.xdg.configHome}/scripts"
      "${config.home.homeDirectory}/.local/bin"
    ];

    sessionVariables = with config.colorscheme.colors; {
      BEMENU_OPTS = "-H 18 -l 5 --fn 'Iosevka FT QP Light 10.5' --tb '#${base0D}' --tf '#${base02}' --hb '#${base0D}' --hf '#${base02}' --nb '#${base02}' --fb '#${base02}'";
      BROWSER = "${pkgs.brave}/bin/brave";
      EDITOR = "${config.programs.neovim.package}/bin/nvim";
      GOPATH = "${config.home.homeDirectory}/Extras/go";
      MANPAGER = "${config.programs.neovim.package}/bin/nvim +Man! -c 'nnoremap i <nop>'";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      RUSTUP_HOME = "${config.home.homeDirectory}/.local/share/rustup";
      XCURSOR_SIZE = "16";
    };
  };

  imports = [
    # Append your custom home-manager modules in this list
    ../../modules/home-manager/wayland/windowManager/river
  ];

  programs = {
    alacritty = {
      enable = true;

      settings = import ./config/alacritty.nix {
        inherit (config) colorscheme;
        isWayland = config.wayland.windowManager.sway.enable;
      };
    };

    dircolors = {
      enable = true;
      settings = pkgs.lib.mkForce { };
      extraConfig = import ./config/dircolors.nix;
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    discocss = {
      enable = true;
      discordAlias = true;
      css = import ./config/discocss-css.nix { inherit (config) colorscheme; };
    };

    emacs = {
      enable = true;

      package = (pkgs.emacsWithPackagesFromUsePackage {
        package = pkgs.emacsPgtkGcc;
        config = ./config/emacs/init.el;

        extraEmacsPackages = epkgs: with epkgs; [
          aggressive-indent
          company
          ctrlf
          doom-themes
          elfeed
          elisp-def
          evil
          exec-path-from-shell
          flycheck
          flycheck-popup-tip
          flycheck-posframe
          gcmh
          helpful
          hide-mode-line
          highlight-defined
          highlight-indent-guides
          highlight-quoted
          hl-todo
          lsp-mode
          lisp-butt-mode
          marginalia
          mixed-pitch
          nix-mode
          no-littering
          olivetti
          prescient
          selectrum
          selectrum-prescient
          simple-modeline
          smartparens
          solaire-mode
          super-save
          which-key
        ];
      });
    };

    exa = {
      enable = true;
      enableAliases = true;
    };

    foot = {
      enable = false;
      server.enable = true;
      settings = import ./config/foot.nix { inherit (config) colorscheme; };
    };

    home-manager.enable = true;

    htop = {
      enable = true;

      settings = {
        detailed_cpu_time = true;
        hide_kernel_threads = false;
        show_cpu_frequency = true;
        show_cpu_usage = true;
        show_program_path = false;
        show_thread_names = true;

        fields = with config.lib.htop.fields; [
          PID
          USER
          PRIORITY
          NICE
          M_SIZE
          M_RESIDENT
          M_SHARE
          STATE
          PERCENT_CPU
          PERCENT_MEM
          TIME
          COMM
        ];
      } // (with config.lib.htop; leftMeters [
        (bar "AllCPUs")
        (bar "Memory")
        (bar "Swap")
      ]) // (with config.lib.htop; rightMeters [
        (bar "Zram")
        (text "Tasks")
        (text "LoadAverage")
        (text "Uptime")
      ]);
    };

    # We don't want to enable it, just set the package so it's convenient for us to use.
    neovim.package = inputs.neovim.packages.${system}.neovim.overrideAttrs (_: {
      __contentAddressed = true;
    });

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    waybar = {
      enable = true;
      package = inputs.nixpkgs-wayland.packages.${system}.waybar;

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

    zsh = {
      enable = true;
      autocd = true;
      enableAutosuggestions = true;
      completionInit = "autoload -U compinit && compinit -d ${config.programs.zsh.dotDir}/zcompdump";
      dotDir = ".config/zsh";

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        path = "${config.programs.zsh.dotDir}/zsh_history";
        save = 50000;
      };

      initExtra = ''
        # You should comment this out, this is useless without my private key
        . /run/agenix/github-token
      '';

      plugins = [{ name = "fast-syntax-highlighting"; src = inputs.zsh-f-sy-h; }];
      shellAliases = import ./config/sh-aliases.nix;
    };
  };

  services = {
    dunst = {
      enable = true;

      package = inputs.nixpkgs-wayland.packages.${system}.dunst.overrideAttrs (old: {
        __contentAddressed = true;

        patches = (old.patches or [ ]) ++ [
          ../../patches/0001-Double-borders-rebased-for-latest-dunst.patch
        ];
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
        dpms = status: "swaymsg 'output * dpms ${status}'";
      in
      {
        enable = true;
        package = inputs.nixpkgs-wayland.packages.${system}.swayidle;

        events = [
          { event = "before-sleep"; command = dpms "off"; }
          # { event = "before-sleep"; command = "swaylock"; }
          { event = "after-resume"; command = dpms "on"; }
          { event = "lock"; command = dpms "off"; }
          { event = "unlock"; command = dpms "on"; }
        ];

        timeouts = [
          { timeout = 300; command = dpms "off"; resumeCommand = dpms "on"; }
          # { timeout = 310; command = "swaylock"; }
        ];
      };
  };

  systemd.user.startServices = "sd-switch";

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

  wayland.windowManager.sway = {
    enable = true;

    package = inputs.nixpkgs-wayland.packages.${system}.sway-unwrapped.overrideAttrs (old: {
      __contentAddressed = true;
    });

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
    enable = true;

    configFile = {
      "emacs" = {
        recursive = true;
        source = ./config/emacs;
      };

      "networkmanager-dmenu/config.ini".source = (pkgs.formats.ini { }).generate "config.ini" {
        dmenu.dmenu_command = "bemenu ${config.home.sessionVariables.BEMENU_OPTS}";
        editor.terminal = "${pkgs.alacritty}/bin/alacritty";
      };

      "nvim" = {
        recursive = true;
        source = ./config/neovim;
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
          let inherit (inputs.nixpkgs-wayland.packages.${system}) wlogout;
          in "${wlogout}/share/wlogout/icons";
      };
    };

    dataFile = {
      "nvim/site/pack/packer/start/impatient.nvim" = {
        recursive = true;
        source = inputs.impatient-nvim;
      };

      "nvim/site/pack/packer/start/packer.nvim" = {
        recursive = true;
        source = inputs.packer-nvim;
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
