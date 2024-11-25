{ config, pkgs, ... }:

{
  programs = {
    dircolors = {
      enable = true;
      enableFishIntegration = config.programs.fish.enable;

      extraConfig = __readFile (
        pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/trapd00r/LS_COLORS/3d833761506d6396f8331fcd11a32d9e3ad3ee80/LS_COLORS";
          hash = "sha256-r70V0JvQ/zlI/uYZ33OGl99qphCXtAgj6+Y3TXbJTLU=";
        }
      );
    };

    direnv = {
      enable = true;
      silent = true;
      nix-direnv.enable = true;
    };

    fish = {
      enable = true;
      shellAbbrs = {
        ls = "eza";
        ll = "eza -l";
        la = "eza -a";
        lt = "eza --tree";
        lla = "eza -la";
        mv = "mv -i";
        cp = "cp -i";
        rm = "rm -i";
        vi = "nvim";
        vim = "nvim";
        vimdiff = "nvim -d";
      };

      interactiveShellInit = ''
        set -U fish_greeting
      '';
    };

    starship = {
      enable = true;
      settings = {
        command_timeout = 3000;
        format = "$username$hostname$nix_shell$character";
        right_format = "$directory$git_branch$git_commit$git_state$git_status";

        character = {
          success_symbol = "[♥](bold green)";
          error_symbol = "[♥](bold red)";
          vimcmd_symbol = "[♡](bold purple)";
          vimcmd_replace_symbol = "[♡](bold green)";
          vimcmd_replace_one_symbol = "[♡](bold green)";
          vimcmd_visual_symbol = "[♡](bold yellow)";
        };

        username = {
          format = "[$user]($style) ";
          disabled = false;
          show_always = true;
        };

        hostname = {
          ssh_only = true;
          ssh_symbol = "";
          format = "at [$hostname](bold blue) ";
          disabled = false;
        };

        git_commit.format = ''( [\($hash$tag\)]($style))'';
        git_state.format = " [\\($state( $progress_current/$progress_total)\\)]($style)";

        git_status = {
          ahead = "↑";
          behind = "↓";
          conflicted = "±";
          deleted = "×";
          diverged = "↕";
          modified = "‼";
          renamed = "≡";
          stashed = "⌂";
          format = ''( [\[$all_status$ahead_behind\]]($style))'';
        };

        git_branch = {
          format = " → [$symbol$branch(:$remote_branch)]($style)";
          symbol = "";
        };

        battery.disabled = true;
        line_break.disabled = true;

        directory = {
          read_only = "(ro)";
          format = "[$read_only]($read_only_style) [$path]($style)";
        };

        nix_shell.format = "[(\\($name\\))]($style) ";
      };
    };

    zoxide = {
      enable = true;
      enableFishIntegration = config.programs.fish.enable;
    };
  };
}
