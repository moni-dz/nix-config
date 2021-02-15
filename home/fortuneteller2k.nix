{ config, lib, pkgs, ... }:

let
  theme = (import ../config/theme.nix);
in rec {
  programs = {
    alacritty = {
      enable = true;
      settings = (import ./config/alacritty.nix {
        inherit theme;
      });
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
      options = (import ./config/zathura.nix {
        inherit theme;
      });
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
      settings = (import ./config/dunst.nix {
        inherit theme;
      });
    };
    mpd = {
      enable = true;
      musicDirectory = "${home.homeDirectory}/Music";
      extraConfig = (import ./config/mpd.nix);
    };
    polybar = {
      enable = true;
      script = "polybar main &";
      config = (import ./config/polybar.nix {
        inherit pkgs theme;
      });
    };
  };
  gtk = {
    enable = true;
    font.name = "Inter";
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };
    theme = {
      package = pkgs.phocus;
      name = "phocus";
    };
  };
  home = {
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
      krita
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
    file = {
      ".config/qt5ct/colors/Horizon.conf".source = ./config/Horizon.conf;
      ".icons/default".source = "${pkgs.gnome3.adwaita-icon-theme}/share/icons/Adwaita";
    };
    sessionPath = [ "\${xdg.configHome}/emacs/bin" ];
    username = "fortuneteller2k";
    homeDirectory = "/home/${home.username}";
    stateVersion = "21.03";
  };
  fonts.fontconfig.enable = true;
  xresources.extraConfig = (import ./config/xresources.nix {
    inherit theme;
  });
}
