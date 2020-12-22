{ config, lib, pkgs, ... }:

{
  programs = {
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = pkgs.emacsPgtkGcc;
      extraPackages = epkgs: with epkgs; [ vterm pdf-tools ];
    };
    vscode = {
      enable = true;
      package = pkgs.vscodium;
      userSettings = (import ./config/vscode.nix);
    };
    alacritty = {
      enable = true;
      settings = (import ./config/alacritty.nix);
    };
    starship = {
      enable = true;
      settings = {
        format = "[fortuneteller2k](bold purple) at [superfluous](bold blue) in $all ";
        line_break.disabled = true;
      };
    };
  };

  systemd.user.services = {
    emacs = {
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.emacsPgtkGcc}/bin/emacs --fg-daemon";
        ExecStop = "${pkgs.emacsPgtkGcc}/bin/emacsclient --eval (kill-emacs)";
        Restart = "on-failure";
      };
      Install.WantedBy = [ "graphical-session.target" ];
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
      settings = (import ./config/dunst.nix);
    };
    polybar = {
      enable = true;
      script = "polybar main &";
      config = (import ./config/polybar.nix);
      package = pkgs.polybar.override {
        i3Support = false;
        i3GapsSupport = false;
        alsaSupport = false;
        iwSupport = false;
        githubSupport = false;
        mpdSupport = false;
        nlSupport = false;
        pulseSupport = true;
      };
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
      package = pkgs.dracula-theme;
      name = "Dracula";
    };
  };

  home = {
    packages = with pkgs; [
      gitAndTools.gh
      neofetch
      peek
      htop
      exa
      brave
      hyperfine
      discord
      discocss
      nix-top
      speedtest-cli
      graphviz
      inkscape
      geogebra6
      sxiv
      texlive.combined.scheme-medium
      obs-studio
      mpv
      papirus-icon-theme
      (zathura.override { useMupdf = false; })
      emacs-all-the-icons-fonts
    ];
    username = "fortuneteller2k";
    homeDirectory = "/home/fortuneteller2k";
    stateVersion = "21.03";
  };
  fonts.fontconfig.enable = true;
}
