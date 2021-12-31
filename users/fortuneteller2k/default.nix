{ config, inputs, lib, pkgs, ... }:

{
  colorscheme = inputs.nix-colors.colorSchemes.horizon-terminal-dark;

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
        then "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ"
        else "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ-AA"
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
    };

    /*
      NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

      Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
    */
    stateVersion = "21.05";
  };

  programs = {
    alacritty = {
      enable = true;

      settings = import ./config/alacritty.nix {
        inherit (config) colorscheme;
        isWayland = true;
      };
    };

    bat = {
      enable = true;
      config = {
        pager = "never";
        style = "plain";
        theme = "base16";
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
      package = pkgs.emacsGcc;
    };

    exa = {
      enable = true;
      enableAliases = true;
    };

    foot = {
      enable = true;
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

    nix-index.enable = true;

    nixvim = {
      enable = true;
      package = pkgs.neovim-nightly;
      colorscheme = "horizon";

      extraPlugins = with pkgs; with vimPlugins; [
        cmp-buffer
        cmp-cmdline
        cmp-path
        cmp-vsnip
        nvim-cmp
        vim-elixir
        vim-vsnip

        (vimUtils.buildVimPlugin {
          name = "vim-horizon";
          src = vim-horizon-src;
        })

        (vimUtils.buildVimPlugin {
          name = "zen-mode-nvim";
          src = zen-mode-nvim-src;
        })
      ];

      plugins = {
        gitgutter.enable = true;
        lualine.enable = true;

        lsp = {
          enable = true;
          servers.rnix-lsp.enable = true;
        };

        lspsaga.enable = true;

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
        mouse = "a";
        number = true;
        termguicolors = true;
      };

      extraConfigLua = ''
        local cmp = require'cmp'

        cmp.setup({
          snippet = {
            -- REQUIRED - you must specify a snippet engine
            expand = function(args)
              vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            end,
          },
          
          mapping = {
            ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
            ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
            ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
            ['<C-y>'] = cmp.config.disable,
            ['<C-e>'] = cmp.mapping({
              i = cmp.mapping.abort(),
              c = cmp.mapping.close(),
            }),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
          },

          sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'vsnip' }, -- For vsnip users.
          }, {
            { name = 'buffer' },
          })
        })
      '';
    };

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    waybar = {
      enable = true;

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
  };

  systemd.user.startServices = "sd-switch";

  wayland.windowManager.sway = {
    enable = true;
    package = null; # Using the NixOS module

    config = {
      bars = [{ command = "${pkgs.waybar}/bin/waybar"; }];
      keybindings = { };
    };

    extraConfig = import ./config/sway.nix {
      inherit (config) colorscheme;
      inherit pkgs;
    };
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
