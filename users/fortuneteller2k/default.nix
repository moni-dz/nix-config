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

    gtk2.extraConfig = "gtk-cursor-theme-size=64";
    gtk3.extraConfig."gtk-cursor-theme-size" = 64;
    gtk4.extraConfig."gtk-cursor-theme-size" = 64;
  };

  home = {
    file = {
      ".local/bin/can" = {
        executable = true;
        text = import ./scripts/can.nix;
      };

      ".local/bin/ccolor" = {
        executable = true;
        text = import ./scripts/ccolor.nix { inherit pkgs; };
      };

      ".local/bin/palette" = {
        executable = true;
        text = import ./scripts/palette.nix;
      };

      ".local/bin/volume" = {
        executable = true;
        text = import ./scripts/volume.nix;
      };

      ".icons/default".source = "${
        if config.colorscheme.kind == "light"
        then "${pkgs.phinger-cursors}/share/icons/phinger-cursors-light"
        else "${pkgs.phinger-cursors}/share/icons/phinger-cursors"
      }";
    };

    homeDirectory = "/home/${config.home.username}";
    username = "fortuneteller2k";

    packages = with pkgs; [
      brave
      clang
      celluloid
      dragon-drop
      element-desktop
      ffmpeg
      font-manager
      gimp
      gitAndTools.gh
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
      nixpkgs-fmt
      nixpkgs-review
      notify-desktop
      nvd
      playerctl
      python3
      ragenix
      rnix-lsp
    ];

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
      XCURSOR_SIZE = "64";
    };

    /*
      NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

      Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
    */
    stateVersion = "21.05";
  };

  programs = {
    alacritty = {
      enable = false;

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
      package = pkgs.neovim-nightly;
      colorscheme = "material";

      extraPlugins = with pkgs.vimPlugins; [
        vim-elixir
        material-nvim
        zen-mode-nvim
      ];

      plugins = {
        treesitter = {
          enable = true;
          ensureInstalled = [ "nix" ];
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
      plugins = import ./config/zsh/plugins.nix { inherit pkgs; };
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
        locker = "swaylock -c ${config.colorscheme.colors.base00} --font 'Sarasa UI J'";
        dpms = status: "swaymsg 'output * dpms ${status}'";
      in
      {
        enable = true;

        events = [
          { event = "before-sleep"; command = dpms "off"; }
          { event = "before-sleep"; command = locker; }
          { event = "after-resume"; command = dpms "on"; }
          { event = "lock"; command = dpms "off"; }
          { event = "unlock"; command = dpms "on"; }
        ];

        timeouts = [
          { timeout = 300; command = dpms "off"; resumeCommand = dpms "on";  }
          { timeout = 310; command = locker; }
        ];
      };
  };

  systemd.user.startServices = "sd-switch";

  wayland.windowManager.sway = {
    enable = true;
    package = null; # Using the NixOS module

    config = with config.colorscheme.colors; rec {
      bars = [ ];
      modifier = "Mod1";
      terminal = "${config.programs.foot.package}/bin/footclient";
      menu = "${pkgs.bemenu}/bin/bemenu-run -H 18 -l 5 --fn 'Iosevka FT QP 10.5' --tb '#${base0B}' --tf '#${base02}' --hb '#${base0B}' --hf '#${base02}' --nb '#${base02}' --fb '#${base02}'";

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
          volume = action: "exec ~/.local/bin/volume ${action}";
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
      smart_borders off
    '';
  };

  xdg = {
    enable = true;

    userDirs = {
      enable = true;
      documents = "${config.home.homeDirectory}/Extras/Documents";
      music = "${config.home.homeDirectory}/Media/Music";
      pictures = "${config.home.homeDirectory}/Media/Pictures";
      videos = "${config.home.homeDirectory}/Media/Videos";
    };
  };
}
