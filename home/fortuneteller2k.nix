{ config, lib, pkgs, ... }:

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
      betterdiscordctl
      bpytop
      brave
      discord
      element-desktop
      exa
      flavours
      fzf
      geogebra6
      gimp
      gitAndTools.gh
      graphviz
      hakuneko
      hyperfine
      inkscape
      kdenlive
      krita
      libreoffice
      mpc_cli
      mpv-with-scripts
      neofetch
      nix-top
      nixpkgs-fmt
      obs-studio
      onefetch
      ox
      peek
      pfetch
      picard
      qutebrowser
      ripcord
      sacad
      speedtest-cli
      sxiv
      texlive.combined.scheme-medium
      ueberzug
      ytmdl
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
    emacs = {
      enable = true;
      package = pkgs.emacsPgtk;
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
      package = pkgs.master.ncmpcpp;
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
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      dotDir = ".config/zsh";
      history = {
        expireDuplicatesFirst = true;
        extended = true;
        path = "${programs.zsh.dotDir}/zsh_history";
        save = 50000;
      };
      initExtra = (import ./config/zshrc.nix);
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
      musicDirectory = "${home.homeDirectory}/Media/Music";
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
      script = "polybar main &";
      config = (import ./config/polybar.nix { inherit pkgs theme; });
    };
  };
  xresources.extraConfig = (import ./config/xresources.nix { inherit theme; });
}
