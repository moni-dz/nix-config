{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;  

  # Let Home Manager install and manage itself.
  programs = { 
    home-manager.enable = true;

    emacs = {
      enable = true;
      package = pkgs.emacsGccPgtk;
      extraPackages = epkgs: [ epkgs.vterm ];
    };

    vscode = {
      enable = true;
      package = pkgs.vscodium;
    };

    alacritty = {
      enable = true;
      settings = {
        font = {
          normal.family = "FantasqueSansMono Nerd Font";
          size = 10.0;
        };
        cursor.style = "Beam";
        colors = (import ./config/alacritty-colors.nix);
      };
    };

    starship = {
      enable = true;
      settings.format = "[fortuneteller2k](bold purple) at [nixos](bold blue) in $directory$nix_shell$git_branch$git_commit$character ";
    };
  };

  services = {
    sxhkd = {
      enable = true;
      extraPath = "/run/current-system/sw/bin";
      keybindings = {
        "Print" = "~/.config/spectrwm/scripts/screenshot.sh wind";
      };
    };

    polybar = {
      enable = true;
      script = "polybar main &";
      config = (import ./config/polybar.nix);
    };
  };

  systemd.user.services = {
    emacs = {
      Service = {
        Type = "forking";
        ExecStart = "${pkgs.emacsGccPgtk}/bin/emacs --daemon";
        ExecStop = "${pkgs.emacsGccPgtk}/bin/emacsclient --eval (kill-emacs)";
        Restart = "always";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };

  gtk = {
    enable = true;
    font.name = "Inter V";
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };
    theme = {
      package = pkgs.dracula-theme;
      name = "Dracula";
    };
  };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    packages = with pkgs; [
      rustup
      rust-analyzer
      neofetch
      htop
      brave
      discord-ptb
      dmenu
      st
      tabbed
      networkmanager_dmenu
      obs-studio
      mpv
      papirus-icon-theme
      (zathura.override { useMupdf = false; })
      emacs-all-the-icons-fonts
    ];

    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";

    sessionVariables = {
      EDITOR = "emacsclient -nc";
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "21.03";
  };

  fonts.fontconfig.enable = true;
}
