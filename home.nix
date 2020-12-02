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
      userSettings = {
        "editor.tabSize" = 2;
        "editor.fontLigatures" = true;
        "editor.semanticHighlighting.enabled" = "true";
        "rust-client.autoStartRls" = true;
        "rust-client.engine" = "rls";
        "workbench.colorTheme" = "Horizon Bold";
        "rust.racer_completion" = true;
        "rust.clippy_preference" = "on";
        "rust.all_features" = true;
        "files.autoSave" = "afterDelay";
        "workbench.iconTheme" = "vscode-icons";
      };
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

  systemd.user.services = {
    emacs = {
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.emacsGccPgtk}/bin/emacs --fg-daemon";
        ExecStop = "${pkgs.emacsGccPgtk}/bin/emacsclient --eval (kill-emacs)";
        Restart = "on-failure";
      };
      Install.WantedBy = [ "graphical-session.target" ];
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
      nodejs
      nodePackages.typescript
      nodePackages.npm
      neofetch
      htop
      exa
      go
      brave
      hyperfine
      discord
      ripcord
      st
      nix-top
      dmenu
      geogebra6
      sxiv
      tabbed
      texlive.combined.scheme-medium
      networkmanager_dmenu
      obs-studio
      mpv
      gimp
      papirus-icon-theme
      (zathura.override { useMupdf = false; })
      emacs-all-the-icons-fonts
    ];
    username = builtins.getEnv "USER";
    homeDirectory = builtins.getEnv "HOME";
    sessionVariables = {
      EDITOR = "emacsclient -nc";
      PATH = "$PATH:$HOME/.config/emacs/bin";
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
