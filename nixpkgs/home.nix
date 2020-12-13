{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;  

  # Let Home Manager install and manage itself.
  programs = { 
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = pkgs.emacsPgtkGcc;
      extraPackages = epkgs: [ epkgs.vterm ];
    };
    vscode = {
      enable = true;
      package = pkgs.vscodium;
      userSettings = {
        "editor.tabSize" = 2;
        "editor.fontLigatures" = true;
        "editor.semanticHighlighting.enabled" = true;
        "rust-analyzer.serverPath" = "/run/current-system/sw/bin/rust-analyzer";
        "workbench.colorTheme" = "Horizon Bold";
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
        background_opacity = 0.95;
        cursor.style = "Beam";
        colors = (import ./config/alacritty-colors.nix);
      };
    };
    starship = {
      enable = true;
      settings = {
        format = "[fortuneteller2k](bold purple) at [nixos](bold blue) in $all ";
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
      settings = {
        global = {
          padding = 24;
          markup = "full";
          alignment = "center";
          word_wrap = "yes";
          horizontal_padding = 24;
          frame_width = 6;
          format = "<b>%s</b>: %b";
          geometry = "300x5-14+40";
          transparency = 10;
          frame_color = "#fab795";
          font = "FantasqueSansMono Nerd Font Mono 10";
          timeout = 10;
        };
        urgency_normal = {
          foreground = "#fdf0ed";
          background = "#16161c";
          frame-color = "#ee64ae";
          timeout = 15;
        };
        urgency_critical = {
          foreground = "#16161c";
          background = "#e95678";
          frame-color = "#16161c";
        };
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

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    packages = with pkgs; [
      neofetch
      htop
      exa
      brave
      hyperfine
      discord
      discocss
      nix-top
      speedtest-cli
      geogebra6
      sxiv
      texlive.combined.scheme-medium
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
