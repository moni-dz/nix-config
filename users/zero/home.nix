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
        neofetch
        nixpkgs-fmt;

      inherit (pkgs.gitAndTools) gh;
      inherit (inputs.agenix.packages.${system}) agenix;
      inherit (config.programs.neovim) package;
      inherit (inputs.statix.packages.${system}) statix;
    };

    sessionVariables = {
      EDITOR = "${config.programs.neovim.package}/bin/nvim";
      MANPAGER = "${config.programs.neovim.package}/bin/nvim +Man! -c 'nnoremap i <nop>'";
    };
  };

  programs = {
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

    exa = {
      enable = true;
      enableAliases = true;
    };

    # We don't want to enable it, just set the package so it's convenient for us to use.
    neovim.package = inputs.neovim.packages.${system}.neovim.overrideAttrs (_: {
      __contentAddressed = true;
    });

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    zsh = {
      enable = true;
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
      shellAliases = import ../shared/sh-aliases.nix;
    };
  };

  xdg = {
    enable = true;

    configFile."nvim" = {
      recursive = true;
      source = ./config/neovim;
    };

    dataFile = {
      "nvim/site/pack/packer/start/impatient.nvim" = {
        recursive = true;
        source = inputs.impatient-nvim;
      };

      "nvim/site/pack/packer/start/packer.nvim" = {
        recursive = true;
        source = inputs.packer-nvim;
      };
    };
  };
}
