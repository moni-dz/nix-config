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
        gitoxide
        nixpkgs-fmt
        nixpkgs-review;

      inherit (pkgs.gitAndTools) gh;
      inherit (config.programs.neovim) package;
      inherit (inputs.agenix.packages.${system}) agenix;
      inherit (inputs.statix.packages.${system}) statix;
    };

    sessionVariables = {
      EDITOR = "emacs -nw";
      MANPAGER = "nvim +Man! -c 'nnoremap i <nop>'";
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

    nixvim = {
      enable = true;
      globals.material_style = "darker";

      options = {
        clipboard = "unnamedplus";
        completeopt = "menu,menuone,noselect";
        guifont = "monospace:h11";
        laststatus = "0";
        mouse = "a";
        modelines = "0";
        ruler = false;
        number = false;
        termguicolors = true;
      };

      plugins.nix.enable = true;

      extraPlugins = lib.attrValues {
        inherit (pkgs.vimPlugins) material-nvim;
      };

      extraConfigLua = ''
        require("material").setup({
          high_visibility = {
            lighter = false,
	    darker = true
	  },
        })

        vim.cmd "colorscheme material"
        vim.cmd "set guicursor=a:ver30"
      '';
    };

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
      shellAliases = import ./config/sh-aliases.nix;
    };
  };

  systemd.user.startServices = "sd-switch";
  xdg.enable = true;
}
