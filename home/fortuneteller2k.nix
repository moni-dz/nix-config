{ config, lib, pkgs, ... }:

let
  theme = (import ../config/theme.nix);
in rec {
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
      ".icons/default".source = "${pkgs.gnome3.adwaita-icon-theme}/share/icons/Adwaita";
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
    emacs = {
      enable = true;
      package = pkgs.emacsPgtk;
    };
    home-manager.enable = true;
    htop.enable = true;
    ncmpcpp = {
      enable = true;
      package = pkgs.ncmpcpp;
      settings = (import ./config/ncmpcpp.nix);
    };
    neovim = {
      enable = true;
      package = pkgs.neovim-unwrapped;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      extraConfig = (import ./config/neovim.nix);
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
    polybar = {
      enable = true;
      script = "polybar main &";
      config = (import ./config/polybar.nix { inherit pkgs theme; });
    };
  };
  xresources.extraConfig = (import ./config/xresources.nix { inherit theme; });
}
