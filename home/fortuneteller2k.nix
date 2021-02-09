{ config, lib, pkgs, ... }:

rec {
  programs = {
    home-manager.enable = true;
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
    neovim = {
      enable = true;
      package = pkgs.neovim-unwrapped;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      extraConfig = (import ./config/neovim.nix);
    };
    alacritty = {
      enable = true;
      settings = (import ./config/alacritty.nix {
        colors = (import ../config/colors.nix);
      });
    };
    starship = {
      enable = true;
      settings = (import ./config/starship.nix);
    };
    chromium = {
      enable = true;
      package = pkgs.ungoogled-chromium;
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
        colors = (import ../config/colors.nix);
      });
    };
    ncmpcpp = {
      enable = true;
      package = pkgs.ncmpcpp;
      settings = (import ./config/ncmpcpp.nix);
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
        colors = (import ../config/colors.nix);
      });
    };
    emacs.enable = true;
    polybar = {
      enable = true;
      script = "polybar main &";
      config = (import ./config/polybar.nix);
    };
    mpd = {
      enable = true;
      musicDirectory = "${home.homeDirectory}/Music";
      extraConfig = ''
        audio_output {
          type "pulse"
          name "mpd pulse-audio-output"
        }
        audio_output {
          type "fifo"
          name "mpd visualizer-fifo"
          path "/tmp/mpd.fifo"
          format "44100:16:2"
        }
      '';
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
      emacs-all-the-icons-fonts
      exa
      master.flavours
      fzf
      geogebra6
      gimp
      gitAndTools.gh
      graphviz
      hakuneko
      htop
      hyperfine
      inkscape
      krita
      mpc_cli
      mpv-with-scripts
      neofetch
      nix-top
      obs-studio
      master.ox
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
    colors = (import ../config/colors.nix);
  });
}
