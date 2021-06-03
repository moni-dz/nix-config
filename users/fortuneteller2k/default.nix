{ config, inputs, lib, pkgs, ... }:

let theme = import ../../config/theme.nix;
in {
  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    font.name = "Sarasa Gothic J";

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name =
        "${if theme.lightModeEnabled then "Papirus-Light" else "Papirus-Dark"}";
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

      ".local/bin/screenshot" = {
        executable = true;
        text = import ./scripts/screenshot.nix { inherit theme; };
      };

      ".local/bin/showcase" = {
        executable = true;
        text = import ./scripts/showcase.nix;
      };

      ".local/bin/volume" = {
        executable = true;
        text = import ./scripts/volume.nix;
      };

      ".config/qt5ct/colors/Horizon.conf".source = ./config/Horizon.conf;

      ".icons/default".source = "${if theme.lightModeEnabled then
        "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ"
      else
        "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ-AA"}";
    };

    homeDirectory = "/home/${config.home.username}";
    username = "fortuneteller2k";

    packages = with pkgs; [
      agenix
      betterdiscord-installer
      brave
      cargo
      celluloid
      discord-canary
      dragon-drop
      element-desktop
      ffmpeg
      flavours
      font-manager
      giph
      gimp
      gitAndTools.gh
      glava
      graphviz
      hydra-check
      hyperfine
      imv
      lazygit
      libimobiledevice
      libirecovery
      manix
      neochat
      neofetch
      nixpkgs-fmt
      nixpkgs-review
      nur.repos.fortuneteller2k.impure.eww
      nvd
      pfetch
      playerctl
      python3
      qutebrowser
      rustc
      rust-analyzer
      rnix-lsp
      speedtest-cli
      spotify-wrapped
      sublime4
      torrential
      ueberzug
      wezterm
      zls
    ];

    sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
      "${config.xdg.configHome}/scripts"
      "${config.home.homeDirectory}/.local/bin"
    ];

    sessionVariables = {
      BROWSER = "${pkgs.brave}/bin/brave";
      EDITOR = "${config.programs.neovim.package}/bin/nvim";
      GOPATH = "${config.home.homeDirectory}/Extras/go";
      MANPAGER = "${config.programs.neovim.package}/bin/nvim +Man!";
      QT_QPA_PLATFORMTHEME = "qt5ct";
      RUSTUP_HOME = "${config.home.homeDirectory}/.local/share/rustup";
    };

    /* NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

       Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
    */
    stateVersion = "21.05";
  };

  programs = {
    alacritty = {
      enable = true;
      settings = import ./config/alacritty.nix { inherit theme; };
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
      enableNixDirenvIntegration = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacsPgtkGcc;
    };

    exa = {
      enable = true;
      enableAliases = true;
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
      } // (with config.lib.htop;
        leftMeters {
          AllCPUs = modes.Bar;
          Memory = modes.Bar;
          Swap = modes.Bar;
          Zram = modes.Bar;
        }) // (with config.lib.htop;
          rightMeters {
            Tasks = modes.Text;
            LoadAverage = modes.Text;
            Uptime = modes.Text;
            Systemd = modes.Text;
          });
    };

    mako = {
      enable = config.wayland.windowManager.sway.enable;
      extraConfig = import ./config/mako.nix { inherit theme; };
    };

    ncmpcpp = {
      enable = config.services.mpd.enable;
      settings = import ./config/ncmpcpp.nix;
    };

    neovim = {
      enable = true;
      package = pkgs.neovim-nightly;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      extraConfig = import ./config/neovim;
      extraPackages = with pkgs; [ rnix-lsp shellcheck ];

      plugins = import ./config/neovim/plugins.nix {
        inherit pkgs;
        inherit (theme) colors;
      };
    };

    nix-index.enable = true;
    rofi.enable = true;

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    qutebrowser = {
      enable = true;
      extraConfig = import ./config/qutebrowser.nix;
    };

    vscode = let extraPackages = pkgs: with pkgs; [ rustup ];
    in {
      enable = true;
      package = pkgs.vscode.fhsWithPackages (pkgs: extraPackages pkgs);
    };

    waybar = {
      enable = config.wayland.windowManager.sway.enable;

      settings = [{
        layer = "bottom";
        position = "top";
        height = 17;
        modules-left = [ "sway/workspaces" "sway/mode" ];
        modules-right = [ "battery" "pulseaudio" "network" "clock" ];
        modules = import ./config/waybar/modules.nix;
      }];

      style = import ./config/waybar/style.nix { inherit theme; };
    };

    zathura = {
      enable = true;
      extraConfig = "map <C-i> recolor";
      options = import ./config/zathura.nix { inherit theme; };
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

      initExtra = import ./config/zsh {
        inherit dotDir;
        inherit (config) home;
      };

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

      settings = import ./config/dunst.nix { inherit theme; };
    };

    mpd = {
      enable = true;
      package = pkgs.master.mpd;
      musicDirectory = config.xdg.userDirs.music;
      extraConfig = import ./config/mpd.nix;
    };

    mpdris2 = {
      enable = true;
      multimediaKeys = true;
      notifications = true;
    };

    playerctld.enable = true;

    polybar = {
      enable = true;
      script = "polybar main &";
      config = import ./config/polybar.nix { inherit pkgs theme; };
    };
  };

  systemd.user.startServices = true;

  wayland.windowManager.sway = {
    enable = false;
    package = null;

    config = {
      keybindings = { };
      bars = [{ command = "${pkgs.waybar}/bin/waybar"; }];
    };

    extraConfig = import ./config/sway.nix { inherit theme; };
  };

  xdg = {
    enable = true;

    configFile = {
      "nvim/coc-settings.json".source = let
        json = pkgs.formats.json { };
        neovim-coc-settings =
          import ./config/neovim/coc-settings.nix { inherit pkgs; };
      in json.generate "coc-settings.json" neovim-coc-settings;

      "wezterm/colors/nix-colors.toml".source = let
        toml = pkgs.formats.toml { };
        wezterm-colors =
          import ./config/wezterm-colors.nix { inherit (theme) colors; };
      in toml.generate "nix-colors.toml" wezterm-colors;

      "wezterm/wezterm.lua".text = ''
        local w = require 'wezterm';

        return {
          confirm = false,
          enable_tab_bar = false,
          default_cursor_style = "BlinkingBar",
          color_scheme = "nix-colors",
          font = w.font("Iosevka FT Light"),
          font_size = 10.5,
          dpi = 96.0,

          window_padding = {
            left = 8,
            right = 8,
            top = 14,
            bottom = 8,
          },

          freetype_load_target = "HorizontalLcd",
          freetype_render_target = "HorizontalLcd",
          freetype_interpreter_version = 40,

          skip_close_confirmation_for_processes_named = {
            "bash", "sh", "zsh", "fish", "tmux"
          },
        }
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

  xresources.extraConfig = import ./config/xresources.nix { inherit theme; };
}
