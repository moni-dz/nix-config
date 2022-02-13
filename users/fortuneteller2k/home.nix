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

      package = (inputs.nixpkgs-f2k.packages.${system}.phocus.override {
        inherit (config.colorscheme) colors;
        primary = config.colorscheme.colors.base08;
        secondary = config.colorscheme.colors.base0B;
      }).overrideAttrs (_: { __contentAddressed = true; });
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
        autotiling
        bemenu
        brave-nightly
        brightnessctl
        clang
        celluloid
        dragon-drop
        ffmpeg
        font-manager
        gimp
        graphviz
        hydra-check
        hyperfine
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
      inherit (inputs.nixpkgs-f2k.packages.${system}) eww-wayland;

      inherit (inputs.nixpkgs-wayland.packages.${system})
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
      MANPAGER = "${config.programs.nixvim.package}/bin/nvim +Man! -c 'nnoremap i <nop>'";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      RUSTUP_HOME = "${config.home.homeDirectory}/.local/share/rustup";
      XCURSOR_SIZE = "16";
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
      enable = false;

      package = (pkgs.emacsWithPackagesFromUsePackage {
        package = pkgs.emacsPgtkGcc;
        config = ./config/emacs.org;

        extraEmacsPackages = epkgs: with epkgs; [
          aggressive-indent
          company
          ctrlf
          doom-themes
          eglot
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
          highlight-quoted
          hl-todo
          lisp-butt-mode
          marginalia
          mixed-pitch
          nix-mode
          no-littering
          olivetti
          prescient
          selectrum
          selectrum-prescient
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

    nixvim = {
      enable = true;

      package = inputs.neovim.packages.${system}.neovim.overrideAttrs (_: {
        __contentAddressed = true;
      });

      colorscheme = "material";

      extraPlugins = with pkgs.vimPlugins; [
        vim-elixir
        vim-nixhash
        material-nvim
        nvim-colorizer-lua
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

        require('colorizer').setup({ '*' }, {
          names    = true;
          RRGGBBAA = true;
          css      = true;
          mode     = 'background';
        })
      '';
    };

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
          modules-right = [ "pulseaudio" "network" "clock" ];
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

        patches = (old.patches or []) ++ [
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

  wayland.windowManager.sway = {
    enable = true;

    package = inputs.nixpkgs-wayland.packages.${system}.sway-unwrapped.overrideAttrs (_: {
      __contentAddressed = true;
      src = inputs.sway_borders;
    });

    config = with config.colorscheme.colors; rec {
      bars = [ ];
      defaultWorkspace = "workspace number 1";
      modifier = "Mod1";
      terminal = "${config.programs.alacritty.package}/bin/alacritty";

      menu = ''
        ${pkgs.bemenu}/bin/bemenu-run -H 18 -l 5 --fn 'Iosevka FT QP Light 10.5' --tb '#${base08}' --tf '#${base02}' --hb '#${base08}' --hf '#${base02}' --nb '#${base02}' --fb '#${base02}'
      '';

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
        outer = -4;
        inner = 18;
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
          "${modifier}+Shift+q" = "exec swaymsg exit";
          "${modifier}+Print" = grimshot "copy" "area";
          "${modifier}+0" = "workspace number 10";
          "${modifier}+Shift+0" = "move container to workspace number 10";
          "${modifier}+b" = "exec pkill -USR1 waybar";
          "${modifier}+c" = "split h";
          "${modifier}+v" = "split v";
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
        border_image = ../../assets/border.png;
      in ''
        exec_always autotiling

        border_images.unfocused ${border_image}
        border_images.focused ${border_image}
        border_images.focused_inactive ${border_image}
        border_images.urgent ${border_image}
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
      "emacs/init.org" = {
        source = ./config/emacs.org;

        onChange = ''
          emacs --batch --load org --eval '(org-babel-tangle-file "~/.config/emacs/init.org")'
        '';
      };

      "networkmanager-dmenu/config.ini".source =
        let
          ini = pkgs.formats.ini { };

          menu = with config.colorscheme.colors; ''
            ${pkgs.bemenu}/bin/bemenu -H 18 -l 5 --fn 'Iosevka FT QP Light 10.5' --tb '#${base08}' --tf '#${base02}' --hb '#${base08}' --hf '#${base02}' --nb '#${base02}' --fb '#${base02}'
          '';
        in
        ini.generate "config.ini" {
          dmenu.dmenu_command = menu;
          editor.terminal = "${pkgs.alacritty}/bin/alacritty";
        };

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
