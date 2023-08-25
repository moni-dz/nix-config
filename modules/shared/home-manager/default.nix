{ config, inputs, inputs', lib, pkgs, system, ... }:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  home = {
    packages = __attrValues {
      inherit (pkgs)
        bat
        ripgrep
        difftastic
        gitoxide
        nixpkgs-fmt
        nixpkgs-review;

      inherit (pkgs.gitAndTools) gh;
      inherit (inputs'.agenix.packages) agenix;
    };

    sessionVariables = {
      MANPAGER = "nvim +Man! -c 'nnoremap i <nop>'";
    };
  };

  programs = {
    dircolors = {
      enable = true;
      enableFishIntegration = config.programs.fish.enable;

      extraConfig = __readFile (pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/trapd00r/LS_COLORS/3d833761506d6396f8331fcd11a32d9e3ad3ee80/LS_COLORS";
        hash = "sha256-r70V0JvQ/zlI/uYZ33OGl99qphCXtAgj6+Y3TXbJTLU=";
      });
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    exa.enable = true;

    git = {
      enable = true;

      aliases = {
        df = "difftool";
        a = "add";
        p = "push";
        r = "rebase";
        ri = "rebase -i";
        cm = "commit";
        pl = "pull";
        s = "status";
        st = "stash";
        ck = "checkout";
        rl = "reflog";
      };

      extraConfig = {
        diff.tool = "difftastic";
        pager.difftool = true;

        difftool = {
          prompt = false;
          difftastic.cmd = ''${lib.getExe pkgs.difftastic} "$LOCAL" "$REMOTE"'';
        };
      };
    };

    home-manager = {
      enable = true;
      path = "${inputs.home}";
    };

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

    nix-index-database.comma.enable = lib.mkDefault true;

    neovim = {
      enable = true;
      defaultEditor = true;

      plugins = __attrValues {
        inherit (pkgs.vimPlugins)
          which-key-nvim
          nvim-lspconfig
          nvim-cmp
          cmp-nvim-lsp
          cmp-vsnip
          cmp-path
          cmp-buffer
          popup-nvim
          plenary-nvim
          telescope-nvim
          rust-tools-nvim
          rose-pine;

        nvim-treesitter = pkgs.vimPlugins.nvim-treesitter.withAllGrammars;

        auto-dark-mode-nvim = pkgs.vimUtils.buildVimPluginFrom2Nix {
          name = "auto-dark-mode-nvim";

          src = pkgs.fetchFromGitHub {
            owner = "f-person";
            repo = "auto-dark-mode.nvim";
            rev = "7d15094390f1a0638a5e533022e99a6aa503dbdf";
            hash = "sha256-f3AJukU9osmHFAWxmSEAw5/GsQyBXDVPdW3eUJJSNpM=";
          };
        };

        inlay-hints-nvim = pkgs.vimUtils.buildVimPluginFrom2Nix {
          name = "inlay-hints-nvim";

          src = pkgs.fetchFromGitHub {
            owner = "simrat39";
            repo = "inlay-hints.nvim";
            rev = "006b0898f5d3874e8e528352103733142e705834";
            hash = "sha256-cDWx08N+NhN5Voxh8f7RGzerbAYB5FHE6TpD4/o/MIQ=";
          };
        };
      };

      package = pkgs.neovim-unwrapped.override {
        lua = pkgs.luajit;
      };

      extraLuaConfig = __readFile ./config/init.lua;
    };

    starship = {
      enable = true;
      settings = import ./config/starship.nix;
    };

    zoxide = {
      enable = true;
      enableFishIntegration = config.programs.fish.enable;
    };

    fish = {
      enable = true;
      shellAbbrs = import ./config/sh-aliases.nix;

      interactiveShellInit = ''
        set -U fish_greeting
      '';
    };
  };
}
