{ config, inputs, lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://rycee.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://rycee.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = lib.attrValues {
      inherit (pkgs)
        bat
        difftastic
        gitoxide
        nixpkgs-fmt
        nixpkgs-review;

      inherit (pkgs.gitAndTools) gh;
      inherit (config.programs.neovim) package;
      inherit (inputs.agenix.packages.${system}) agenix;
      inherit (inputs.statix.packages.${system}) statix;
    };

    sessionVariables = {
      MANPAGER = "sh -c 'col -bx | bat --theme ansi -l man -p'";
      MANROFFOPT = "-c";
    };
  };

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    exa = {
      enable = true;
      enableAliases = true;
    };

    git = {
      enable = true;
      aliases."dft" = "difftool";

      extraConfig = {
        diff.tool = "difftastic";
        difftool.prompt = false;
        difftool."difftastic".cmd = ''${lib.getExe pkgs.difftastic} "$LOCAL" "$REMOTE"'';
        pager.difftool = true;
      };
    };

    home-manager.enable = true;

    htop = {
      enable = true;

      settings = {
        detailed_cpu_time = true;
        hide_kernel_threads = false;
        show_cpu_frequency = true;
        show_cpu_usage = true;
        show_program_path = false;
        show_thread_names = true;

        fields = with config.lib.htop.fields; [
          PID
          USER
          PRIORITY
          NICE
          M_SIZE
          M_RESIDENT
          M_SHARE
          STATE
          PERCENT_CPU
          PERCENT_MEM
          TIME
          COMM
        ];
      } // (with config.lib.htop; leftMeters [
        (bar "AllCPUs")
        (bar "Memory")
        (bar "Swap")
      ]) // (with config.lib.htop; rightMeters [
        (text "Tasks")
        (text "LoadAverage")
        (text "Uptime")
      ]);
    };

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    zsh = {
      enable = !pkgs.stdenv.isDarwin;
      autocd = true;
      enableAutosuggestions = true;

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        save = 50000;
      };

      initExtra = ''
        # You should comment this out, this is useless without my private key
        . /run/agenix/github-token
      '';

      plugins = [{ name = "fast-syntax-highlighting"; src = inputs.zsh-f-sy-h; }];
      shellAliases = import ./config/sh-aliases.nix;
    };
  };

}
