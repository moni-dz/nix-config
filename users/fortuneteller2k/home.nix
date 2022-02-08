{ config, inputs, lib, pkgs, ... }:

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
      package = pkgs.phocus;
      name = "phocus";
    };

    gtk2.extraConfig = "gtk-cursor-theme-size=32";
    gtk3.extraConfig."gtk-cursor-theme-size" = 32;
    gtk4.extraConfig."gtk-cursor-theme-size" = 32;
  };

  home = {
    file.".icons/default".source = "${
      if config.colorscheme.kind == "light"
      then "${pkgs.phinger-cursors}/share/icons/phinger-cursors-light"
      else "${pkgs.phinger-cursors}/share/icons/phinger-cursors"
    }";

    packages = lib.attrValues {
      inherit (pkgs)
        autotiling
        bemenu
        brave
        brightnessctl
        clang
        celluloid
        dragon-drop
        element-desktop
        ffmpeg
        font-manager
        gimp
        graphviz
        hydra-check
        hyperfine
        imagemagick
        imv
        inkscape
        jq
        lazygit
        libimobiledevice
        libirecovery
        multimc-offline
        nixpkgs-fmt
        nixpkgs-review
        notify-desktop
        nvd
        playerctl
        wayland-utils
        xdg_utils;

      inherit (pkgs.gitAndTools) gh;
      inherit (pkgs.sway-contrib) grimshot;
      inherit (inputs.agenix.packages.${pkgs.system}) agenix;
      inherit (inputs.nixpkgs-f2k.packages.${pkgs.system}) eww-wayland;

      inherit (inputs.nixpkgs-wayland.packages.${pkgs.system})
        grim
        slurp
        swaybg
        swayidle
        swaylock
        wf-recorder
        wl-clipboard;

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

      sway-active-ws = pkgs.writers.writeDashBin "sway-active-ws" (import ./scripts/sway-active-ws.nix);
      volume = pkgs.writers.writeDashBin "volume" (import ./scripts/volume.nix);
    };

    sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
      "${config.xdg.configHome}/scripts"
      "${config.home.homeDirectory}/.local/bin"
    ];

    sessionVariables = {
      BROWSER = "${pkgs.brave}/bin/brave";
      EDITOR = "${config.programs.nixvim.package}/bin/nvim";
      GOPATH = "${config.home.homeDirectory}/Extras/go";
      MANPAGER = "${config.programs.nixvim.package}/bin/nvim +Man!";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      RUSTUP_HOME = "${config.home.homeDirectory}/.local/share/rustup";
      XCURSOR_SIZE = "32";
    };
  };

  programs = {
    alacritty = {
      enable = false;
      package = inputs.nixpkgs-f2k.packages.${pkgs.system}.alacritty-ligatures;

      settings = import ./config/alacritty.nix {
        inherit (config) colorscheme;
        isWayland = true;
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
      discord = pkgs.discord-openasar;
      discordAlias = true;
      css = import ./config/discocss-css.nix { inherit (config) colorscheme; };
    };

    emacs = {
      enable = false;
      package = pkgs.emacsPgtkGcc;
    };

    exa = {
      enable = true;
      enableAliases = true;
    };

    foot = {
      enable = true;
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

    ncmpcpp = {
      enable = config.services.mpd.enable;
      settings = import ./config/ncmpcpp.nix;
    };

    nixvim = {
      enable = true;
      package = inputs.neovim.packages.${pkgs.system}.neovim;
      colorscheme = "material";

      extraPlugins = with pkgs.vimPlugins; [
        vim-elixir
        vim-nixhash
        material-nvim
        zen-mode-nvim
      ];

      plugins = {
        lsp = {
          enable = true;

          servers = {
            pyright.enable = true;
            rust-analyzer.enable = true;
            rnix-lsp.enable = true;
          };
        };

        lspsaga.enable = true;

        treesitter = {
          enable = true;
          ensureInstalled = [ "nix" "rust" ];
        };

        nix.enable = true;
      };

      options = {
        clipboard = "unnamedplus";
        completeopt = "menu,menuone,noselect";
        guifont = "monospace:h11";
        guicursor = "a:ver25-iCursor";
        laststatus = "0";
        mouse = "a";
        modelines = "0";
        ruler = false;
        showmode = true;
        number = false;
        termguicolors = true;
      };

      extraConfigVim = ''
        let g:material_style = 'darker'
      '';

      extraConfigLua = ''
        require('material').setup({
          italics = {
            comments = true,
            keywords = false,
            functions = false,
            strings = false,
            variables = false
          },

          high_visibility = {
            lighter = false,
            darker = true
          },
        })
      '';
    };

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    waybar = {
      enable = false;
      package = inputs.nixpkgs-wayland.packages.${pkgs.system}.waybar;

      settings = [
        {
          layer = "bottom";
          position = "top";
          height = 17;
          modules-left = [ "sway/workspaces" "sway/mode" ];
          modules-right = [ "pulseaudio" "network" "clock" ];
          modules = import ./config/waybar/modules.nix;
        }
      ];

      style = import ./config/waybar/style.nix { inherit (config) colorscheme; };
    };

    zathura = {
      enable = true;
      extraConfig = "map <C-i> recolor";
      options = import ./config/zathura.nix { inherit (config) colorscheme; };
    };

    zsh = rec {
      enable = true;
      autocd = true;
      enableAutosuggestions = true;
      dotDir = ".config/zsh";

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        path = "${config.programs.zsh.dotDir}/zsh_history";
        save = 50000;
      };

      initExtra = import ./config/zsh { inherit dotDir; };
      plugins = import ./config/zsh/plugins.nix { inherit inputs; };
      shellAliases = import ./config/sh-aliases.nix;
    };
  };

  services = {
    dunst = {
      enable = true;

      iconTheme = {
        name = "Papirus";
        size = "32x32";
        package = pkgs.papirus-icon-theme;
      };

      settings = import ./config/dunst.nix { inherit (config) colorscheme; };
    };

    mpd = {
      enable = true;
      package = pkgs.master.mpd;
      musicDirectory = config.xdg.userDirs.music;
      extraConfig = import ./config/mpd.nix;
    };

    mpdris2 = {
      enable = config.services.mpd.enable;
      multimediaKeys = true;
      notifications = true;
    };

    playerctld.enable = true;

    swayidle =
      let
        dpms = status: "swaymsg 'output * dpms ${status}'";
      in
      {
        enable = true;
        package = inputs.nixpkgs-wayland.packages.${pkgs.system}.swayidle;

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

  wayland.windowManager.sway = {
    enable = true;
    package = inputs.nixpkgs-wayland.packages.${pkgs.system}.sway-unwrapped;
    wrapperFeatures.gtk = true;

    config = with config.colorscheme.colors; rec {
      bars = [ ];
      modifier = "Mod1";
      terminal = "${config.programs.foot.package}/bin/footclient";
      menu = "${pkgs.bemenu}/bin/bemenu-run -H 18 -l 5 --fn 'Iosevka FT QP 10.5' --tb '#${base08}' --tf '#${base02}' --hb '#${base08}' --hf '#${base02}' --nb '#${base02}' --fb '#${base02}'";

      colors =
        let
          inactive = {
            background = "#${base02}";
            border = "#${base02}";
            childBorder = "#${base02}";
            indicator = "#${base02}";
            text = "#${base00}";
          };
        in
        {
          inherit (inactive) background;

          focused = {
            background = "#${base08}";
            border = "#${base08}";
            childBorder = "#${base08}";
            indicator = "#${base08}";
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

        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
        };
      };

      gaps = {
        inner = 8;
        smartGaps = false;
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
          "${modifier}+Shift+q" = "exec swaymsg exit";
          "${modifier}+Print" = grimshot "copy" "area";
          "Print" = grimshot "copy" "active";
          "Control+Print" = grimshot "copy" "screen";
          "Mod4+Print" = grimshot "save" "screen";
          "XF86AudioRaiseVolume" = volume "up";
          "XF86AudioLowerVolume" = volume "down";
          "XF86AudioMute" = volume "toggle";
          "XF86MonBrightnessUp" = brightness "up";
          "XF86MonBrightnessDown" = brightness "down";
        };

      output."*".bg = "#${base00} solid_color";

      window = {
        border = 4;
        hideEdgeBorders = "smart";
      };
    };

    extraConfig = ''
      exec_always autotiling
      exec_always eww kill
      exec_always eww daemon
      exec_always eww open active-workspace-indicator
      smart_borders off
    '';

    extraSessionCommands = ''
      export XDG_SESSION_DESKTOP=sway
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland-egl
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export MOZ_ENABLE_WAYLAND=1
      export CLUTTER_BACKEND=wayland
      export ECORE_EVAS_ENGINE=wayland-egl
      export ELM_ENGINE=wayland_eg
      export NO_AT_BRIDGE=1
    '';
  };

  xdg = {
    enable = true;

    configFile = {
      "eww/eww.scss".text = import ./config/eww-scss.nix { inherit (config) colorscheme; };
      "eww/eww.yuck".text = import ./config/eww-config.nix;

      "swaylock/config".text = ''
        daemonize
        ignore-empty-password
        show-failed-attempts
        color=${config.colorscheme.colors.base00}
        font=Sarasa UI J
      '';
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
