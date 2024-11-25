{
  config,
  inputs,
  inputs',
  lib,
  pkgs,
  ...
}:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  imports = [ ./fish.nix ];

  home = {
    packages = __attrValues {
      inherit (pkgs)
        bat
        ripgrep
        difftastic
        nixpkgs-fmt
        nixpkgs-review
        ;

      inherit (pkgs.gitAndTools) gh;
      inherit (inputs'.agenix.packages) agenix;
      inherit (inputs'.nvim.packages) neovim;
    };

    sessionVariables = {
      EDITOR = "nano";
      MANPAGER = "nvim +Man! -c 'nnoremap i <nop>'";
    };
  };

  programs = {
    eza = {
      enable = true;
      package = pkgs.eza;
    };

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
      path = lib.mkForce "${inputs.home}";
    };

    htop = {
      enable = true;

      settings =
        {
          detailed_cpu_time = true;
          hide_kernel_threads = false;
          show_cpu_frequency = pkgs.stdenv.isLinux;
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
        }
        // (
          with config.lib.htop;
          leftMeters [
            (bar "AllCPUs")
            (bar "Memory")
            (bar "Swap")
          ]
        )
        // (
          with config.lib.htop;
          rightMeters [
            (text "Tasks")
            (text "LoadAverage")
            (text "Uptime")
          ]
        );
    };

    nix-index-database.comma.enable = lib.mkDefault true;
  };
}
