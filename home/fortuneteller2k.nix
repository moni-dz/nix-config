{ config, lib, pkgs, ... }:

{
  programs = {
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = pkgs.emacsGit;
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
        format = "[fortuneteller2k](bold red) at [superfluous](bold blue) in $all ";
        line_break.disabled = false;
      };
    };
    qutebrowser = {
      enable = true;
      extraConfig = (import ./config/qutebrowser.nix);
    };
    zathura = {
      enable = true;
      package = pkgs.zathura;
      extraConfig = "map <C-i> recolor";
      options = (import ./config/zathura.nix);
    };
  };
  services = {
    emacs = {
      enable = true;
      client = {
        enable = true;
        arguments = [ "-n" "-c" ];
      };
    };
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
      weechat-unwrapped
      pfetch
      bpytop
      w3m
      gitAndTools.gh
      qutebrowser
      neofetch
      peek
      htop
      exa
      brave
      hyperfine
      discord
      #betterdiscordctl
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
      hakuneko
      papirus-icon-theme
      emacs-all-the-icons-fonts
      ytmdl
      # eww
    ];
    username = "fortuneteller2k";
    homeDirectory = "/home/fortuneteller2k";
    stateVersion = "21.03";
  };
  fonts.fontconfig.enable = true;
}
