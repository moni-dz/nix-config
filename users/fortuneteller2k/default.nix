{ config, inputs, lib, pkgs, ... }:

let
  theme = import ../../config/theme.nix;
in
{
  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    font.name = "Sarasa Gothic J";

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "${if theme.lightModeEnabled then "Papirus-Light" else "Papirus-Dark"}";
    };

    theme = {
      package = pkgs.phocus;
      name = "phocus";
    };
  };

  home = {
    file = {
      ".config/nvim/coc-settings.json".source =
        let
          json = pkgs.formats.json { };
          neovim-coc-settings = import ./config/neovim/coc-settings.nix { inherit pkgs; };
        in
        json.generate "coc-settings.json" neovim-coc-settings;

      ".config/wezterm/colors/nix-colors.toml".source =
        let
          toml = pkgs.formats.toml { };
          wezterm-colors = import ./config/wezterm-colors.nix { inherit (theme) colors; };
        in
        toml.generate "nix-colors.toml" wezterm-colors;

      ".local/bin/can" = {
        executable = true;
        text = import ./scripts/can.nix;
      };

      ".local/bin/ccolor" = {
        executable = true;
        text = import ./scripts/ccolor.nix;
      };

      ".local/bin/palette" = {
        executable = true;
        text = import ./scripts/palette.nix;
      };

      ".local/bin/screenshot" = {
        executable = true;
        text = import ./scripts/screenshot.nix;
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

      ".icons/default".source = "${
        if theme.lightModeEnabled
        then "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ"
        else "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ-AA"
      }";
    };

    homeDirectory = "/home/${config.home.username}";
    username = "fortuneteller2k";

    packages = with pkgs; [
      betterdiscord-installer
      brave
      cargo
      celluloid
      discord
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
      manix
      neofetch
      nixpkgs-fmt
      nixpkgs-review
      nur.repos.fortuneteller2k.impure.eww
      pfetch
      playerctl
      qutebrowser
      rustc
      rust-analyzer
      rnix-lsp
      speedtest-cli
      spotify-wrapped
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
      BROWSER = "brave";
      EDITOR = "nvim";
    };

    stateVersion = "21.03";
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
      detailedCpuTime = true;
      hideKernelThreads = false;
      showCpuFrequency = true;
      showCpuUsage = true;
      showProgramPath = false;
      showThreadNames = true;
      meters = {
        left = [ "AllCPUs" "Memory" "Swap" ];
        right = [ "Tasks" "LoadAverage" "Uptime" ];
      };
    };

    mako = {
      enable = true;
      extraConfig = import ./config/mako.nix { inherit theme; };
    };

    ncmpcpp = {
      enable = true;
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

    rofi.enable = true;

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    qutebrowser = {
      enable = true;
      extraConfig = import ./config/qutebrowser.nix;
    };

    waybar = {
      enable = true;

      settings = [
        {
          layer = "bottom";
          position = "top";
          height = 17;
          modules-left = [ "sway/workspaces" "sway/mode" ];
          modules-right = [ "battery" "pulseaudio" "network" "clock" ];
          modules = import ./config/waybar/modules.nix;
        }
      ];

      style = import ./config/waybar/style.nix { inherit theme; };
    };


    zathura = {
      enable = true;
      package = pkgs.zathura;
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

      initExtra = import ./config/zshrc.nix {
        inherit dotDir;
        inherit (config) home;
      };

      plugins = [
        rec {
          name = "fast-syntax-highlighting";
          src = pkgs.fetchFromGitHub {
            owner = "zdharma";
            repo = name;
            rev = "a62d721affc771de2c78201d868d80668a84c1e1";
            sha256 = "sha256-4xJXH9Wn18/+Vfib/ZrhCRp/yB1PppsbZCx1/WafmU8=";
          };
        }
        {
          name = "doas";
          src = pkgs.fetchFromGitHub {
            owner = "anatolykopyl";
            repo = "doas-zsh-plugin";
            rev = "17d0b55ca2acd12f7acc9e38c4ecaf413725be18";
            sha256 = "sha256-10rcF9cho9GuZCFQVIdFjvHCAlTLHNaLY4twxjT2jcE=";
          };
        }
      ];

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
      musicDirectory = "${config.xdg.userDirs.music}";
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
    enable = true;
    package = null;

    config = {
      keybindings = { };
      bars = [{ command = "${pkgs.waybar}/bin/waybar"; }];
    };

    extraConfig = import ./config/sway.nix { inherit theme; };
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

  xresources.extraConfig = import ./config/xresources.nix { inherit theme; };
}
