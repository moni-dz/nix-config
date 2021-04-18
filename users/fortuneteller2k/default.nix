{ config, inputs, lib, pkgs, ... }:

let
  theme = import ../../config/theme.nix;
in
rec {
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
    activation.reloadPolybar = "${pkgs.polybar}/bin/polybar-msg cmd restart || echo 'skipping...'";

    file = {
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

    homeDirectory = "/home/${home.username}";
    username = "fortuneteller2k";

    packages = with pkgs; [
      betterdiscordctl
      bpytop
      brave
      cava
      celluloid
      cbonsai
      cmatrix
      comma
      discord
      dragon-drop
      element-desktop
      ffmpeg
      flavours
      font-manager
      fzf
      gimp
      gitAndTools.gh
      glava
      graphviz
      hakuneko
      hyperfine
      inkscape
      jetbrains.idea-ultimate
      kdenlive
      krita
      lazygit
      manix
      mpc_cli
      neofetch
      nixfmt
      nix-top
      nixpkgs-fmt
      nixpkgs-review
      nur.repos.fortuneteller2k.ytmdl
      nur.repos.fortuneteller2k.impure.eww
      obs-studio
      onefetch
      ox
      peek
      pfetch
      picard
      playerctl
      qutebrowser
      ripcord
      sacad
      speedtest-cli
      spotify-adblock
      sxiv
      tdesktop
      texlive.combined.scheme-medium
      torrential
      ueberzug
      wezterm
      woeusb
    ];

    sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
      "${config.xdg.configHome}/scripts"
      "${home.homeDirectory}/.local/bin"
    ];

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
      showCpuFrequency = true;
      showCpuUsage = true;
      showProgramPath = false;
      showThreadNames = true;
      meters = {
        left = [ "AllCPUs" "Memory" "Swap" ];
        right = [ "Tasks" "LoadAverage" "Uptime" ];
      };
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
      extraConfig = import ./config/neovim.nix;
      extraPackages = with pkgs; [ rnix-lsp shellcheck ];
    };

    newsboat = {
      enable = true;
      autoReload = true;

      urls = [
        {
          tags = [ "repology" "nixos" "nixpkgs" ];
          title = "Nixpkgs Repology";
          url = "https://repology.org/maintainer/lythe1107%40gmail.com/feed-for-repo/nix_unstable/atom";
        }
        {
          tags = [ "nixos" "nixpkgs" ];
          title = "NixOS Weekly";
          url = "https://weekly.nixos.org/feeds/all.rss.xml";
        }
      ];

      extraConfig = ''
        color listnormal color3 color0
        color listfocus color5 color0
        color listnormal_unread color0 color3
        color info  red default bold
        color listfocus_unread color0 color5 bold
        color info color4 color0 
      '';
    };

    rofi.enable = true;

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    termite = with theme.colors; {
      enable = true;
      allowBold = true;
      audibleBell = true;
      backgroundColor = termiteBg;
      clickableUrl = true;

      colorsExtra = ''
        color0 = #${c0}
        color1 = #${c1}
        color2 = #${c2}
        color3 = #${c3}
        color4 = #${c4}
        color5 = #${c5}
        color6 = #${c6}
        color7 = #${c7}
        color8 = #${c8}
        color9 = #${c9}
        color10 = #${c10}
        color11 = #${c11}
        color12 = #${c12}
        color13 = #${c13}
        color14 = #${c14}
        color15 = #${c15}
      '';

      cursorBlink = "on";
      cursorColor = "#${fg}";
      cursorForegroundColor = "#${fg}";
      cursorShape = "ibeam";
      dynamicTitle = true;
      filterUnmatchedUrls = true;
      font = "Iosevka FT 11";
      foregroundBoldColor = "#${fg}";
      foregroundColor = "#${fg}";
      fullscreen = true;
      highlightColor = "#${activeBorderColor}";
      hintsActiveBackgroundColor = "#${activeBorderColor}";
      hintsBorderColor = "#${activeBorderColor}";
      hintsActiveForegroundColor = "#${activeBorderColor}";
      hintsPadding = 8;
    };

    qutebrowser = {
      enable = true;
      extraConfig = import ./config/qutebrowser.nix;
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
        path = "${programs.zsh.dotDir}/zsh_history";
        save = 50000;
      };

      initExtra = import ./config/zshrc.nix { inherit dotDir home; };

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
      package = pkgs.head.mpd;
      musicDirectory = "${xdg.userDirs.music}";
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

  xdg = {
    enable = true;

    userDirs = {
      enable = true;
      documents = "${home.homeDirectory}/Extras/Documents";
      music = "${home.homeDirectory}/Media/Music";
      pictures = "${home.homeDirectory}/Media/Pictures";
      videos = "${home.homeDirectory}/Media/Videos";
    };
  };

  xresources.extraConfig = import ./config/xresources.nix { inherit theme; };
}
