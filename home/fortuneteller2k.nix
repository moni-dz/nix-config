{ config, lib, pkgs, ... }:

{
  programs = {
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = pkgs.emacsPgtkGcc;
      extraPackages = epkgs: with epkgs; [ vterm pdf-tools ];
    };
    urxvt = {
      enable = true;
      fonts = [ "xft:monospace:size=10:antialias=true" ];
      scroll.bar.enable = false;
      iso14755 = false;
      extraConfig = {
        letterSpace = 0;
        lineSpace = 0;
      };
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
      settings.format =
        "[fortuneteller2k](bold red) at [superfluous](bold blue) in $all ";
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
      name = "fortuneteller2k_phocus";
    };
  };
  home = {
    packages = with pkgs; [
      cmus
      weechat-unwrapped
      pfetch
      bpytop
      gitAndTools.gh
      qutebrowser
      neofetch
      peek
      htop
      exa
      brave
      hyperfine
      discord
      betterdiscordctl
      discocss
      nix-top
      speedtest-cli
      gimp
      krita
      graphviz
      inkscape
      geogebra6
      sxiv
      texlive.combined.scheme-medium
      obs-studio
      mpv-with-scripts
      hakuneko
      emacs-all-the-icons-fonts
      ytmdl
    ];
    sessionPath = [ "\${xdg.configHome}/emacs/bin" ];
    sessionVariables.EDITOR = "emacsclient -nc";
    username = "fortuneteller2k";
    homeDirectory = "/home/fortuneteller2k";
    stateVersion = "21.03";
  };
  fonts.fontconfig.enable = true;
  xresources.extraConfig = (import ./config/xresources.nix);
}
