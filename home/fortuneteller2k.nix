{ config, inputs, lib, pkgs, ... }:

let
  theme = (import ../config/theme.nix);
in
rec {
  fonts.fontconfig.enable = true;
  gtk = {
    enable = true;
    font.name = "Inter";
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
    activation.reloadPolybar = "${pkgs.polybar}/bin/polybar-msg cmd restart";
    file = {
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
      adoptopenjdk-openj9-bin-15
      betterdiscordctl
      bpytop
      brave
      comma
      discord
      element-desktop
      exa
      flavours
      fzf
      geogebra6
      gimp
      gitAndTools.gh
      gradle
      graphviz
      hakuneko
      hyperfine
      inkscape
      jetbrains.idea-ultimate
      kdenlive
      krita
      mpc_cli
      mpv-with-scripts
      neofetch
      nix-top
      nixpkgs-fmt
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
      # sacad # wait for nixpkgs PR to get merged
      speedtest-cli
      sxiv
      texlive.combined.scheme-medium
      ueberzug
    ];
    stateVersion = "21.05";
  };
  programs = {
    alacritty = {
      enable = true;
      settings = (import ./config/alacritty.nix { inherit theme; });
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
      extraConfig = (import ./config/dircolors.nix);
    };
    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
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
      settings = (import ./config/ncmpcpp.nix);
    };
    neovim = {
      enable = true;
      package = pkgs.neovim-nightly;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      extraConfig = (import ./config/neovim.nix);
      extraPackages = with pkgs; [ rnix-lsp shellcheck ];
    };
    starship = {
      enable = true;
      settings = (import ./config/starship.nix);
    };
    qutebrowser = {
      enable = true;
      extraConfig = (import ./config/qutebrowser.nix);
    };
    zathura = {
      enable = true;
      package = pkgs.zathura;
      extraConfig = "map <C-i> recolor";
      options = (import ./config/zathura.nix { inherit theme; });
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
      initExtra = (import ./config/zshrc.nix { inherit dotDir home; });
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
      ];
      shellAliases = (import ./config/sh-aliases.nix);
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
      settings = (import ./config/dunst.nix { inherit theme; });
    };
    mpd = {
      enable = true;
      package = pkgs.stable.mpd;
      musicDirectory = "${xdg.userDirs.music}";
      extraConfig = (import ./config/mpd.nix);
    };
    mpdris2 = {
      enable = true;
      multimediaKeys = true;
      notifications = true;
    };
    playerctld.enable = true;
    polybar = {
      enable = true;
      script = "polybar -l=trace main &";
      config = (import ./config/polybar.nix { inherit pkgs theme; });
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
  xresources.extraConfig = (import ./config/xresources.nix { inherit theme; });
}
